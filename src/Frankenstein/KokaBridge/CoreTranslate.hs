-- | Translation from Koka Core IR to Frankenstein Core IR
--
-- Koka Core is the source of truth for Frankenstein's Core types —
-- this bridge is the closest to an identity function of any bridge.
-- Key gaps handled here:
--   1. Koka Effect (row types) → Frankenstein EffectRow (syntactic)
--   2. Koka TName (Name×Type pair) → separate Name + Type
--   3. Koka multi-scrutinee Case → single-scrutinee ECase
--   4. Multiplicity: defaults to Many (Perceus re-derives)
--   5. Koka ConRepr/DataRepr backend info → discarded

module Frankenstein.KokaBridge.CoreTranslate
  ( translateProgram
  , translateExpr
  , translateType
  ) where

import Data.Text (Text)
import Data.Text qualified as T

import Core.Core qualified as KC
import Type.Type qualified as KT
import Kind.Kind qualified as KK
import Common.Name qualified as KN
import Common.Syntax qualified as KS

import Frankenstein.Core.Types qualified as F

-- ============================================================================
-- Top-level translation
-- ============================================================================

translateProgram :: KC.Core -> Either Text F.Program
translateProgram kcore = do
  defs <- mapM translateDef (KC.flattenDefGroups (KC.coreProgDefs kcore))
  dataDecls <- concat <$> mapM translateTypeDefGroup (KC.coreProgTypeDefs kcore)
  let effects = []  -- TODO: extract effect declarations from type defs
  pure F.Program
    { F.progName    = translateQName (KC.coreProgName kcore)
    , F.progDefs    = defs
    , F.progData    = dataDecls
    , F.progEffects = effects
    }

-- ============================================================================
-- Definition translation
-- ============================================================================

translateDef :: KC.Def -> Either Text F.Def
translateDef kdef = do
  expr <- translateExpr (KC.defExpr kdef)
  ty   <- translateType (KC.defType kdef)
  pure F.Def
    { F.defName       = translateQName (KC.defName kdef)
    , F.defType       = ty
    , F.defExpr       = expr
    , F.defSort       = translateDefSort (KC.defSort kdef)
    , F.defVisibility = translateVisibility (KC.defVis kdef)
    }

-- ============================================================================
-- Expression translation
-- ============================================================================

translateExpr :: KC.Expr -> Either Text F.Expr
translateExpr = \case
  KC.Var tname _varInfo ->
    pure $ F.EVar (translateTNameToName tname)

  KC.Lit lit ->
    pure $ F.ELit (translateLit lit)

  KC.Con tname _conRepr ->
    pure $ F.ECon (translateTNameToQName tname)

  KC.App f args -> do
    f'    <- translateExpr f
    args' <- mapM translateExpr args
    pure $ F.EApp f' args'

  KC.TypeLam tvars body -> do
    body' <- translateExpr body
    let tvars' = map translateTypeVar tvars
    pure $ F.ETypeLam tvars' body'

  KC.TypeApp expr tys -> do
    expr' <- translateExpr expr
    tys'  <- mapM translateType tys
    pure $ F.ETypeApp expr' tys'

  KC.Lam tnames _eff body -> do
    body'   <- translateExpr body
    let params = [(translateTNameToName tn, translateTypeUnsafe (KC.tnameType tn)) | tn <- tnames]
    pure $ F.ELam params body'

  KC.Let defGroups body -> do
    body'  <- translateExpr body
    groups <- mapM translateDefGroup defGroups
    pure $ F.ELet groups body'

  -- Koka Case has [Expr] (multi-scrutinee) and [Branch]
  -- Single scrutinee: direct translation
  KC.Case [scrut] branches -> do
    scrut'    <- translateExpr scrut
    branches' <- mapM translateBranch branches
    pure $ F.ECase scrut' branches'

  -- Multi-scrutinee: nest as tuple-like matching (desugar to first scrutinee for now)
  KC.Case (scrut:_) branches -> do
    scrut'    <- translateExpr scrut
    branches' <- mapM translateBranch branches
    pure $ F.ECase scrut' branches'

  KC.Case [] _ ->
    Left "Empty case scrutinee list"

-- ============================================================================
-- Branch/Pattern translation
-- ============================================================================

translateBranch :: KC.Branch -> Either Text F.Branch
translateBranch (KC.Branch pats guards) = do
  -- Koka has multiple patterns per branch (for multi-scrutinee case)
  -- We take the first pattern for single-scrutinee translation
  pat <- case pats of
    (p:_) -> translatePattern p
    []    -> pure $ F.PatWild anyType
  -- Koka has multiple guards per branch; take the first, or combine
  case guards of
    [KC.Guard test body] -> do
      test' <- translateExpr test
      body' <- translateExpr body
      -- If guard test is True (always-match), no guard needed
      let mguard = if isExprTrue test then Nothing else Just test'
      pure $ F.Branch pat mguard body'
    (KC.Guard _test body : _) -> do
      -- Multiple guards: take first (TODO: desugar to nested if)
      body' <- translateExpr body
      pure $ F.Branch pat Nothing body'
    [] ->
      Left "Branch with no guards"
  where
    isExprTrue (KC.Con tname _) = KN.nameStem (KC.getName tname) == "True"
    isExprTrue _ = False

translatePattern :: KC.Pattern -> Either Text F.Pattern
translatePattern = \case
  KC.PatWild ->
    pure $ F.PatWild anyType

  KC.PatLit lit ->
    pure $ F.PatLit (translateLit lit)

  KC.PatVar tname subPat -> do
    -- PatVar binds a name and has a sub-pattern
    -- If sub-pattern is PatWild, it's just a variable binding
    case subPat of
      KC.PatWild ->
        pure $ F.PatVar (translateTNameToName tname) (translateTypeUnsafe (KC.tnameType tname))
      _ -> do
        -- Named pattern: translate the sub-pattern (the name is an alias)
        -- For now, just use the sub-pattern
        translatePattern subPat

  KC.PatCon tname pats _conRepr _typeArgs _exists _typeRes _conInfo _skip -> do
    pats' <- mapM translatePattern pats
    pure $ F.PatCon (translateTNameToQName tname) pats'

-- ============================================================================
-- Type translation
-- ============================================================================

translateType :: KT.Type -> Either Text F.Type
translateType = \case
  KT.TForall tvars rho -> do
    rho' <- translateType rho
    pure $ F.TForall (map translateTypeVar tvars) rho'

  KT.TFun args eff ret -> do
    args' <- mapM (\(_, ty) -> do
      ty' <- translateType ty
      pure (F.Many, ty')) args  -- Multiplicity defaults to Many; Perceus re-derives
    effRow <- translateEffect eff
    ret'   <- translateType ret
    pure $ F.TFun args' effRow ret'

  KT.TCon tc ->
    pure $ F.TCon (translateTypeCon tc)

  KT.TVar tv ->
    pure $ F.TVar (translateTypeVar tv)

  KT.TApp t1 [t2] -> do
    t1' <- translateType t1
    t2' <- translateType t2
    pure $ F.TApp t1' t2'

  KT.TApp t1 (t2:rest) -> do
    -- Multi-arg TApp: fold left
    t1' <- translateType t1
    t2' <- translateType t2
    foldlM (\acc ty -> do
      ty' <- translateType ty
      pure $ F.TApp acc ty') (F.TApp t1' t2') rest

  KT.TApp t1 [] ->
    translateType t1

  KT.TSyn syn args body -> do
    args' <- mapM translateType args
    body' <- translateType body
    pure $ F.TSyn (translateQName (KT.typesynName syn)) args' body'

-- | Translate Koka effect (row type) to Frankenstein EffectRow
--
-- Koka effects are encoded as row types:
--   effectEmpty = TCon nameEffectEmpty
--   effectExtend label rest = TApp (TApp (TCon nameEffectExtend) [label]) [rest]
--   typeTotal = effectEmpty
translateEffect :: KT.Type -> Either Text F.EffectRow
translateEffect ty =
  case KT.extractEffectExtend ty of
    ([], _tail)
      | isEffectEmpty ty -> pure F.EffectRowEmpty
      | otherwise -> case ty of
          KT.TVar tv -> pure $ F.EffectRowVar (translateTypeVar tv)
          _          -> pure F.EffectRowEmpty  -- conservative fallback
    (labels, tail_) -> do
      tailRow <- translateEffect tail_
      -- Each label is a Type; extract its name for EffectRowExtend
      pure $ foldr (\label acc -> F.EffectRowExtend (effectLabelName label) acc) tailRow labels
  where
    isEffectEmpty (KT.TCon tc) = KN.nameStem (KT.typeconName tc) == "()"
                              || KN.nameLocal (KT.typeconName tc) == "<>"
    isEffectEmpty _            = False

    effectLabelName :: KT.Type -> F.QName
    effectLabelName (KT.TCon tc) = translateQName (KT.typeconName tc)
    effectLabelName (KT.TApp (KT.TCon tc) _) = translateQName (KT.typeconName tc)
    effectLabelName _ = F.QName "unknown" (F.Name "effect" 0)

-- ============================================================================
-- TypeVar / TypeCon translation
-- ============================================================================

translateTypeVar :: KT.TypeVar -> F.TypeVar
translateTypeVar tv = F.TypeVar
  { F.tvName         = F.Name (T.pack (show (KT.typevarId tv))) (KT.typevarId tv)
  , F.tvKind         = translateKind (KT.typevarKind tv)
  , F.tvMultiplicity = F.Many
  }

translateTypeCon :: KT.TypeCon -> F.TypeCon
translateTypeCon tc = F.TypeCon
  { F.tcName = translateQName (KT.typeconName tc)
  , F.tcKind = translateKind (KT.typeconKind tc)
  }

translateKind :: KK.Kind -> F.Kind
translateKind k
  | KK.isKindStar k   = F.KindStar
  | KK.isKindEffect k = F.KindEffectRow
  | KK.isKindLabel k  = F.KindEffect
  | KK.isKindHeap k   = F.KindHeap
  | otherwise = case k of
      KK.KApp (KK.KApp _ k1) k2 -> F.KindArrow (translateKind k1) (translateKind k2)
      KK.KApp _ k'              -> translateKind k'
      _                          -> F.KindValue

-- ============================================================================
-- Data type declarations
-- ============================================================================

translateTypeDefGroup :: KC.TypeDefGroup -> Either Text [F.DataDecl]
translateTypeDefGroup (KC.TypeDefGroup tdefs) =
  concat <$> mapM translateTypeDef tdefs

translateTypeDef :: KC.TypeDef -> Either Text [F.DataDecl]
translateTypeDef = \case
  KC.Data dataInfo -> do
    let conInfos = KT.dataInfoConstrs dataInfo
    cons <- mapM translateConInfo conInfos
    pure [ F.DataDecl
      { F.dataName   = translateQName (KT.dataInfoName dataInfo)
      , F.dataParams = map translateTypeVar (KT.dataInfoParams dataInfo)
      , F.dataCons   = cons
      , F.dataVis    = translateVisibility (KT.dataInfoVis dataInfo)
      } ]
  KC.Synonym _synInfo ->
    -- Type synonyms don't produce data declarations
    pure []

translateConInfo :: KT.ConInfo -> Either Text F.ConDecl
translateConInfo ci = do
  let fields = zip
        (map (\(n, _) -> translateNameK n) (KT.conInfoParams ci))
        (map (\(_, ty) -> translateTypeUnsafe ty) (KT.conInfoParams ci))
  pure F.ConDecl
    { F.conName   = translateQName (KT.conInfoName ci)
    , F.conFields = fields
    , F.conVis    = translateVisibility (KT.conInfoVis ci)
    }

-- ============================================================================
-- DefGroup → BindGroup translation
-- ============================================================================

translateDefGroup :: KC.DefGroup -> Either Text F.BindGroup
translateDefGroup = \case
  KC.DefRec defs -> mapM translateDefToBind defs
  KC.DefNonRec def -> (:[]) <$> translateDefToBind def

translateDefToBind :: KC.Def -> Either Text F.Bind
translateDefToBind kdef = do
  expr <- translateExpr (KC.defExpr kdef)
  ty   <- translateType (KC.defType kdef)
  pure F.Bind
    { F.bindName = translateNameK (KC.defName kdef)
    , F.bindType = ty
    , F.bindExpr = expr
    , F.bindSort = translateDefSort (KC.defSort kdef)
    }

-- ============================================================================
-- Literal translation
-- ============================================================================

translateLit :: KC.Lit -> F.Lit
translateLit = \case
  KC.LitInt i    -> F.LitInt i
  KC.LitFloat d  -> F.LitFloat d
  KC.LitChar c   -> F.LitChar c
  KC.LitString s -> F.LitString (T.pack s)

-- ============================================================================
-- Name translation
-- ============================================================================

-- | Translate Koka TName to Frankenstein Name (discards type info)
translateTNameToName :: KC.TName -> F.Name
translateTNameToName tname = translateNameK (KC.getName tname)

-- | Translate Koka TName to Frankenstein QName (discards type info)
translateTNameToQName :: KC.TName -> F.QName
translateTNameToQName tname = translateQName (KC.getName tname)

-- | Translate a Koka Name to Frankenstein Name (local part only)
translateNameK :: KN.Name -> F.Name
translateNameK kn = F.Name (T.pack (KN.nameLocal kn)) 0

-- | Translate a Koka Name to Frankenstein QName (module-qualified)
translateQName :: KN.Name -> F.QName
translateQName kn = F.QName
  (T.pack (KN.nameModule kn))
  (F.Name (T.pack (KN.nameLocal kn)) 0)

-- ============================================================================
-- Misc translation helpers
-- ============================================================================

translateDefSort :: KS.DefSort -> F.DefSort
translateDefSort = \case
  KS.DefFun {} -> F.DefFun
  KS.DefVal    -> F.DefVal
  KS.DefVar    -> F.DefVar

translateVisibility :: KS.Visibility -> F.Visibility
translateVisibility = \case
  KS.Public  -> F.Public
  KS.Private -> F.Private

-- | Unsafe type translation (for contexts where failure isn't an option)
translateTypeUnsafe :: KT.Type -> F.Type
translateTypeUnsafe ty = case translateType ty of
  Right t  -> t
  Left _   -> anyType

anyType :: F.Type
anyType = F.TCon $ F.TypeCon (F.QName "std" (F.Name "any" 0)) F.KindValue

-- | foldlM implementation
foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM _ acc []     = pure acc
foldlM f acc (x:xs) = do acc' <- f acc x; foldlM f acc' xs
