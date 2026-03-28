-- | Bridge to Koka's actual Core IR
--
-- This module translates between Frankenstein's simplified Core types
-- and Koka's real Core.Core data types, enabling us to feed into Koka's
-- existing optimization pipeline (Perceus, evidence-passing, inlining,
-- etc.) and then emit via either Koka's C backend or our MLIR backend.
--
-- Currently handles the demo subset: ELit, EVar, EApp, ELam, ELet, ECase,
-- ETypeLam, ETypeApp, ECon. Effect operations (EPerform/EHandle) and
-- resource management ops (ERetain/ERelease/EDrop/EReuse) are left as TODOs.

module Frankenstein.Core.KokaCore
  ( toKokaCore
  , fromKokaCore
  ) where

import Frankenstein.Core.Types qualified as F

import Data.Text (Text)
import Data.Text qualified as T

import Core.Core qualified as KC
import Type.Type qualified as KT
import Kind.Kind qualified as KK
import Common.Name qualified as KN
import Common.Range qualified as KR
import Common.Syntax qualified as KS

-- Re-use the reverse translation from KokaBridge
import Frankenstein.KokaBridge.CoreTranslate (translateProgram)

-- ============================================================================
-- fromKokaCore: Koka Core -> Frankenstein Core
-- ============================================================================

-- | Convert Koka Core to Frankenstein Core.
-- This is exactly what KokaBridge.CoreTranslate.translateProgram does,
-- so we delegate to it.
fromKokaCore :: KC.Core -> Either Text F.Program
fromKokaCore = translateProgram

-- ============================================================================
-- toKokaCore: Frankenstein Core -> Koka Core
-- ============================================================================

-- | Convert Frankenstein Core to Koka Core.
--
-- Handles the demo subset: ELit, EVar, EApp, ELam, ELet, ECase,
-- ETypeLam, ETypeApp, ECon. Effect operations and Perceus operations
-- are not yet translated.
toKokaCore :: F.Program -> Either Text KC.Core
toKokaCore prog = do
  defs <- mapM toKokaDef (F.progDefs prog)
  -- TODO: translate DataDecl -> TypeDefGroup
  -- TODO: translate EffectDecl -> TypeDefGroup (effect types)
  pure KC.Core
    { KC.coreProgName     = toKokaQName (F.progName prog)
    , KC.coreProgImports  = []
    , KC.coreProgFixDefs  = []
    , KC.coreProgTypeDefs = []  -- TODO: translate data + effect declarations
    , KC.coreProgDefs     = map KC.DefNonRec defs
    , KC.coreProgExternals = []
    , KC.coreProgDoc      = ""
    }

-- ============================================================================
-- Definition translation
-- ============================================================================

toKokaDef :: F.Def -> Either Text KC.Def
toKokaDef fdef = do
  expr <- toKokaExpr (F.defExpr fdef)
  ty   <- toKokaType (F.defType fdef)
  pure KC.Def
    { KC.defName      = toKokaQName (F.defName fdef)
    , KC.defType      = ty
    , KC.defExpr      = expr
    , KC.defVis       = toKokaVisibility (F.defVisibility fdef)
    , KC.defSort      = toKokaDefSort (F.defSort fdef)
    , KC.defInline    = KS.InlineAuto
    , KC.defNameRange = KR.rangeNull
    , KC.defDoc       = ""
    }

-- ============================================================================
-- Expression translation
-- ============================================================================

toKokaExpr :: F.Expr -> Either Text KC.Expr
toKokaExpr = \case
  F.EVar name ->
    -- Without full type info, use typeUnit as placeholder
    pure $ KC.Var (KC.TName (toKokaName name) anyKokaType) KC.InfoNone

  F.ELit lit ->
    pure $ KC.Lit (toKokaLit lit)

  F.ECon qname ->
    -- Constructor: needs ConRepr, use a generic ConNormal placeholder
    pure $ KC.Con (KC.TName (toKokaQName qname) anyKokaType) placeholderConRepr

  F.EApp f args -> do
    f'    <- toKokaExpr f
    args' <- mapM toKokaExpr args
    pure $ KC.App f' args'

  F.ETypeLam tvars body -> do
    body' <- toKokaExpr body
    let tvars' = map toKokaTypeVar tvars
    pure $ KC.TypeLam tvars' body'

  F.ETypeApp expr tys -> do
    expr' <- toKokaExpr expr
    tys'  <- mapM toKokaType tys
    pure $ KC.TypeApp expr' tys'

  F.ELam params body -> do
    body' <- toKokaExpr body
    let tnames = [ KC.TName (toKokaName n) (toKokaTypeUnsafe ty) | (n, ty) <- params ]
    pure $ KC.Lam tnames KT.typeTotal body'

  F.ELet groups body -> do
    body'   <- toKokaExpr body
    groups' <- mapM toKokaBindGroup groups
    pure $ KC.Let groups' body'

  F.ECase scrut branches -> do
    scrut'    <- toKokaExpr scrut
    branches' <- mapM toKokaBranch branches
    pure $ KC.Case [scrut'] branches'

  -- Effect operations: not yet translatable to Koka Core
  -- (need evidence-passing translation)
  F.EPerform eff _args ->
    Left $ "toKokaCore: EPerform not yet supported (effect: " <> F.nameText (F.qnameName eff) <> ")"

  F.EHandle _ _ _ ->
    Left "toKokaCore: EHandle not yet supported"

  -- Perceus operations should not appear in input to Koka
  -- (Koka's own Perceus pass generates these)
  F.ERetain _ ->
    Left "toKokaCore: ERetain should not appear in pre-Perceus Core"
  F.ERelease _ ->
    Left "toKokaCore: ERelease should not appear in pre-Perceus Core"
  F.EDrop _ ->
    Left "toKokaCore: EDrop should not appear in pre-Perceus Core"
  F.EReuse _ _ ->
    Left "toKokaCore: EReuse should not appear in pre-Perceus Core"

  -- Laziness: desugar to thunk create/force calls
  F.EDelay expr -> do
    expr' <- toKokaExpr expr
    let thunkLam = KC.Lam [] KT.typeTotal expr'
    pure $ KC.App (KC.Var (KC.TName (KN.newQualified "std/core" "kk_thunk_create") anyKokaType) KC.InfoNone) [thunkLam]

  F.EForce expr -> do
    expr' <- toKokaExpr expr
    pure $ KC.App (KC.Var (KC.TName (KN.newQualified "std/core" "kk_thunk_force") anyKokaType) KC.InfoNone) [expr']

-- ============================================================================
-- Branch/Pattern translation
-- ============================================================================

toKokaBranch :: F.Branch -> Either Text KC.Branch
toKokaBranch (F.Branch pat mguard body) = do
  pat'  <- toKokaPattern pat
  body' <- toKokaExpr body
  guard' <- case mguard of
    Nothing -> pure KC.exprTrue
    Just g  -> toKokaExpr g
  pure $ KC.Branch [pat'] [KC.Guard guard' body']

toKokaPattern :: F.Pattern -> Either Text KC.Pattern
toKokaPattern = \case
  F.PatWild _ty ->
    pure KC.PatWild

  F.PatLit lit ->
    pure $ KC.PatLit (toKokaLit lit)

  F.PatVar name ty ->
    pure $ KC.PatVar (KC.TName (toKokaName name) (toKokaTypeUnsafe ty)) KC.PatWild

  F.PatCon qname pats -> do
    pats' <- mapM toKokaPattern pats
    pure $ KC.PatCon
      (KC.TName (toKokaQName qname) anyKokaType)
      pats'
      placeholderConRepr
      []              -- typeArgs
      []              -- exists
      anyKokaType     -- typeRes
      placeholderConInfo
      False           -- skip

-- ============================================================================
-- BindGroup translation
-- ============================================================================

toKokaBindGroup :: F.BindGroup -> Either Text KC.DefGroup
toKokaBindGroup binds = case binds of
  [b] -> KC.DefNonRec <$> toKokaBind b
  bs  -> KC.DefRec <$> mapM toKokaBind bs

toKokaBind :: F.Bind -> Either Text KC.Def
toKokaBind b = do
  expr <- toKokaExpr (F.bindExpr b)
  ty   <- toKokaType (F.bindType b)
  pure KC.Def
    { KC.defName      = toKokaName (F.bindName b)
    , KC.defType      = ty
    , KC.defExpr      = expr
    , KC.defVis       = KS.Private
    , KC.defSort      = toKokaDefSort (F.bindSort b)
    , KC.defInline    = KS.InlineAuto
    , KC.defNameRange = KR.rangeNull
    , KC.defDoc       = ""
    }

-- ============================================================================
-- Type translation
-- ============================================================================

toKokaType :: F.Type -> Either Text KT.Type
toKokaType = \case
  F.TForall tvars rho -> do
    rho' <- toKokaType rho
    pure $ KT.TForall (map toKokaTypeVar tvars) rho'

  F.TFun args effRow ret -> do
    args' <- mapM (\(_mult, ty) -> do
      ty' <- toKokaType ty
      pure (KN.nameNil, ty')) args
    eff <- toKokaEffect effRow
    ret' <- toKokaType ret
    pure $ KT.TFun args' eff ret'

  F.TCon tc ->
    pure $ KT.TCon (toKokaTypeCon tc)

  F.TVar tv ->
    pure $ KT.TVar (toKokaTypeVar tv)

  F.TApp t1 t2 -> do
    t1' <- toKokaType t1
    t2' <- toKokaType t2
    pure $ KT.TApp t1' [t2']

  F.TSyn qname args body -> do
    args' <- mapM toKokaType args
    body' <- toKokaType body
    let syn = KT.TypeSyn (toKokaQName qname) KK.kindStar 0 Nothing
    pure $ KT.TSyn syn args' body'

-- | Convert Frankenstein EffectRow to Koka effect type (row type)
toKokaEffect :: F.EffectRow -> Either Text KT.Type
toKokaEffect = \case
  F.EffectRowEmpty ->
    pure KT.effectEmpty

  F.EffectRowExtend label rest -> do
    rest' <- toKokaEffect rest
    let labelTy = KT.TCon (KT.TypeCon (toKokaQName label) KK.kindLabel)
    pure $ KT.effectExtend labelTy rest'

  F.EffectRowVar tv ->
    pure $ KT.TVar (toKokaTypeVar tv)

-- ============================================================================
-- TypeVar / TypeCon translation
-- ============================================================================

toKokaTypeVar :: F.TypeVar -> KT.TypeVar
toKokaTypeVar tv = KT.TypeVar
  { KT.typevarId      = F.nameUnique (F.tvName tv)
  , KT.typevarKind    = toKokaKind (F.tvKind tv)
  , KT.typevarFlavour = KT.Bound
  }

toKokaTypeCon :: F.TypeCon -> KT.TypeCon
toKokaTypeCon tc = KT.TypeCon
  { KT.typeconName = toKokaQName (F.tcName tc)
  , KT.typeconKind = toKokaKind (F.tcKind tc)
  }

toKokaKind :: F.Kind -> KK.Kind
toKokaKind = \case
  F.KindStar      -> KK.kindStar
  F.KindValue     -> KK.kindStar
  F.KindEffect    -> KK.kindLabel
  F.KindEffectRow -> KK.kindEffect
  F.KindHeap      -> KK.kindHeap
  F.KindArrow k1 k2 -> KK.kindFun (toKokaKind k1) (toKokaKind k2)

-- ============================================================================
-- Literal translation
-- ============================================================================

toKokaLit :: F.Lit -> KC.Lit
toKokaLit = \case
  F.LitInt i    -> KC.LitInt i
  F.LitFloat d  -> KC.LitFloat d
  F.LitChar c   -> KC.LitChar c
  F.LitString s -> KC.LitString (T.unpack s)

-- ============================================================================
-- Name translation
-- ============================================================================

toKokaName :: F.Name -> KN.Name
toKokaName n = KN.newName (T.unpack (F.nameText n))

toKokaQName :: F.QName -> KN.Name
toKokaQName qn =
  let m = T.unpack (F.qnameModule qn)
      n = T.unpack (F.nameText (F.qnameName qn))
  in if null m then KN.newName n else KN.newQualified m n

-- ============================================================================
-- Misc translation helpers
-- ============================================================================

toKokaDefSort :: F.DefSort -> KS.DefSort
toKokaDefSort = \case
  F.DefFun -> KS.DefFun [] KS.noFip
  F.DefVal -> KS.DefVal
  F.DefVar -> KS.DefVar

toKokaVisibility :: F.Visibility -> KS.Visibility
toKokaVisibility = \case
  F.Public  -> KS.Public
  F.Private -> KS.Private

-- | Unsafe type translation (for contexts where failure isn't an option)
toKokaTypeUnsafe :: F.Type -> KT.Type
toKokaTypeUnsafe ty = case toKokaType ty of
  Right t  -> t
  Left _   -> anyKokaType

-- | Placeholder type for when we don't have type information
anyKokaType :: KT.Type
anyKokaType = KT.typeUnit

-- | Placeholder ConRepr for constructor expressions.
-- The real ConRepr requires DataRepr analysis which depends on the full
-- data type being known. Using ConNormal with dummy values.
placeholderConRepr :: KC.ConRepr
placeholderConRepr = KC.ConNormal
  { KC.conTypeName = KN.nameNil
  , KC.conDataRepr = KC.DataNormal False
  , KC.conValRepr  = KS.valueReprZero
  , KC.conCtxPath  = KC.CtxNone
  , KC.conTag      = 0
  }

-- | Placeholder ConInfo for pattern matching.
-- The real ConInfo requires full constructor metadata.
placeholderConInfo :: KT.ConInfo
placeholderConInfo = KT.ConInfo
  { KT.conInfoName       = KN.nameNil
  , KT.conInfoTypeName   = KN.nameNil
  , KT.conInfoForalls    = []
  , KT.conInfoExists     = []
  , KT.conInfoParams     = []
  , KT.conInfoType       = anyKokaType
  , KT.conInfoTypeSort   = KS.Inductive
  , KT.conInfoRange      = KR.rangeNull
  , KT.conInfoParamRanges = []
  , KT.conInfoParamVis   = []
  , KT.conInfoSingleton  = False
  , KT.conInfoOrderedParams = []
  , KT.conInfoValueRepr  = KS.valueReprZero
  , KT.conInfoLazy       = Nothing
  , KT.conInfoTag        = 0
  , KT.conInfoVis        = KS.Private
  , KT.conInfoDoc        = ""
  }
