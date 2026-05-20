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

{-# LANGUAGE NamedFieldPuns #-}
module Frankenstein.KokaBridge.CoreTranslate
  ( translateProgram
  , translateProgramMulti
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

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)

import Frankenstein.Core.Types qualified as F

-- ============================================================================
-- Top-level translation
-- ============================================================================

translateProgram :: KC.Core -> Either Text F.Program
translateProgram kcore = do
  defs <- mapM translateDef (KC.flattenDefGroups (KC.coreProgDefs kcore))
  -- Build extern name mapping: @extern-fibonacci → fibonacci (C target name)
  let externMap = buildExternMap (KC.coreProgExternals kcore)
      -- Rewrite @extern- references in defs to use C function names
      defs' = map (rewriteExternRefs externMap) defs
      -- Remove wrapper defs that are now trivial pass-through to extern.
      -- A def like fibonacci = fn(n) fibonacci(n) after rewriting is just
      -- an identity wrapper that the linker can resolve directly.
      defs'' = filter (not . isExternWrapper externMap) defs'
  dataDecls <- concat <$> mapM translateTypeDefGroup (KC.coreProgTypeDefs kcore)
  let effects = concatMap extractEffectDecls (KC.coreProgTypeDefs kcore)
      -- Inject synthetic DataDecl for std/core/types/list so that
      -- assignProgramTags gives Nil=tag0, Cons=tag1 matching the
      -- runtime's KK_NIL_TAG=0, KK_CONS_TAG=1 convention.
      tyVarA = F.TypeVar (F.Name "a" 0) F.KindStar F.Many
      listDecl = F.DataDecl
        { F.dataName   = F.QName "std/core/types" (F.Name "list" 0)
        , F.dataParams = [tyVarA]
        , F.dataCons   =
            [ F.ConDecl { F.conName = F.QName "std/core/types" (F.Name "Nil" 0)
                        , F.conFields = [], F.conVis = F.Public }
            , F.ConDecl { F.conName = F.QName "std/core/types" (F.Name "Cons" 0)
                        , F.conFields = [ (F.Name "head" 0, F.TVar tyVarA)
                                        , (F.Name "tail" 0, F.TCon (F.TypeCon (F.QName "std/core/types" (F.Name "list" 0)) F.KindStar)) ]
                        , F.conVis = F.Public }
            ]
        , F.dataVis    = F.Public
        }
      allDataDecls = listDecl : dataDecls
  pure F.Program
    { F.progName    = translateQName (KC.coreProgName kcore)
    , F.progDefs    = defs''
    , F.progData    = allDataDecls
    , F.progEffects = effects
    }

-- | Translate a root Koka module plus a list of imported modules into
-- a single Frankenstein Program.  The root drives `progName`; defs,
-- data declarations, and effects from every supplied module are
-- merged together.  Duplicate QNames are de-duped (last-write-wins),
-- which matters when the synthetic `list` decl from translateProgram
-- shows up in both the root's output and an imported module's
-- output.
translateProgramMulti :: KC.Core -> [KC.Core] -> Either Text F.Program
translateProgramMulti rootCore importCores = do
  rootProg <- translateProgram rootCore
  importProgs <- mapM translateProgram importCores
  let allProgs = rootProg : importProgs
      mergedDefs = dedupDefs $ concatMap F.progDefs allProgs
      mergedData = dedupData $ concatMap F.progData allProgs
      mergedEffs = concatMap F.progEffects allProgs
  pure rootProg
    { F.progDefs    = mergedDefs
    , F.progData    = mergedData
    , F.progEffects = mergedEffs
    }
  where
    -- Last-write-wins de-dup keyed by QName.  Imported-module
    -- duplicates are rare but happen when two modules both depend
    -- on the same Koka-stdlib helper that gets re-exported.
    dedupDefs :: [F.Def] -> [F.Def]
    dedupDefs = Map.elems . Map.fromList . map (\d -> (F.defName d, d))
    dedupData :: [F.DataDecl] -> [F.DataDecl]
    dedupData = Map.elems . Map.fromList . map (\d -> (F.dataName d, d))

-- ============================================================================
-- External (extern) declarations → name rewriting
-- ============================================================================

-- | Build a mapping from Koka's internal @extern- names to C function names.
-- Koka generates wrapper defs that call @extern-fibonacci etc.
-- We rewrite those references to the actual C function name so the linker
-- can resolve them to cross-module definitions.
buildExternMap :: [KC.External] -> Map Text Text
buildExternMap exts = Map.fromList
  [ (T.pack (KN.nameLocal (KC.externalName ext)), cName)
  | ext@KC.External{} <- exts
  , let baseName = T.pack (KN.nameStem (KC.externalName ext))
        cName = findCTarget (KC.externalFormat ext) baseName
  ]

-- | Find the C target name from extern format list.
-- Koka extern format strings use #1, #2 for parameters.
-- We strip those and extract just the function name.
findCTarget :: [(KS.Target, String)] -> Text -> Text
findCTarget [] dflt = dflt
findCTarget ((KS.C _, s):_) _ = extractCName (T.pack s)
findCTarget ((KS.Default, s):_) _ = extractCName (T.pack s)
findCTarget (_:rest) dflt = findCTarget rest dflt

-- | Extract the C function name from a Koka extern format string.
-- "fibonacci" → "fibonacci"
-- "fibonacci(#1)" → "fibonacci"
extractCName :: Text -> Text
extractCName s =
  let stripped = T.takeWhile (\c -> c /= '(' && c /= ' ') s
  in if T.null stripped then s else stripped

-- | Is this def a trivial extern wrapper?
-- After rewriting, a def like `fibonacci = fn(n) fibonacci(n)` is just
-- a pass-through to the extern function. We can remove it so the linker
-- resolves calls directly to the cross-module definition.
isExternWrapper :: Map Text Text -> F.Def -> Bool
isExternWrapper externMap d =
  let baseName = F.nameText (F.qnameName (F.defName d))
  in any (\cName -> cName == baseName) (Map.elems externMap)

-- | Rewrite @extern- references in a Def's expression tree.
rewriteExternRefs :: Map Text Text -> F.Def -> F.Def
rewriteExternRefs externMap d =
  d { F.defExpr = rewriteExternExpr externMap (F.defExpr d) }

rewriteExternExpr :: Map Text Text -> F.Expr -> F.Expr
rewriteExternExpr m expr = go expr
  where
    go (F.EVar n) = case Map.lookup (F.nameText n) m of
      Just cName -> F.EVar (n { F.nameText = cName })
      Nothing    -> F.EVar n
    go (F.EApp f args) = F.EApp (go f) (map go args)
    go (F.ELam ps body) = F.ELam ps (go body)
    go (F.ELet bgs body) = F.ELet (map (map goBind) bgs) (go body)
    go (F.ECase s brs) = F.ECase (go s) (map goBranch brs)
    go (F.ERetain e) = F.ERetain (go e)
    go (F.EDrop e) = F.EDrop (go e)
    go (F.ERelease e) = F.ERelease (go e)
    go (F.EReuse a b) = F.EReuse (go a) (go b)
    go (F.EDelay e) = F.EDelay (go e)
    go (F.EForce e) = F.EForce (go e)
    go (F.ETypeApp e ts) = F.ETypeApp (go e) ts
    go (F.ETypeLam tvs e) = F.ETypeLam tvs (go e)
    go (F.EPerform qn args) = F.EPerform qn (map go args)
    go (F.EHandle eff h b) = F.EHandle eff (go h) (go b)
    go (F.EFunRef qn) = F.EFunRef qn
    go e = e  -- ELit, ECon
    goBind b = b { F.bindExpr = go (F.bindExpr b) }
    goBranch br = br { F.branchBody = go (F.branchBody br) }

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
  KC.Var tname _varInfo
    -- Bare-EVar reference to `++` (e.g. passed to a HOF like fold).
    -- Route to `str_concat` so the linker sees a defined symbol.
    | KN.nameStem (KC.getName tname) == "++" ->
        pure $ F.EVar (F.Name "str_concat" 0)
    | KN.nameLocal (KC.getName tname) == "++" ->
        pure $ F.EVar (F.Name "str_concat" 0)
    | otherwise ->
        pure $ F.EVar (translateTNameToName tname)

  KC.Lit lit ->
    pure $ F.ELit (translateLit lit)

  -- Koka Nil constructor → ECon with well-known nil tag
  KC.Con tname _conRepr
    | isKokaNil (KC.getName tname) ->
        pure $ F.EVar (F.Name "kk_nil" 0)  -- will be emitted as kk_nil()
    | isKokaCons (KC.getName tname) ->
        pure $ F.ECon (F.QName "std/core/types" (F.Name "Cons" 0))
    | otherwise ->
        pure $ F.ECon (translateTNameToQName tname)

  -- Koka stdlib builtins: intercept known functions and translate to
  -- direct runtime calls rather than higher-order closure applications.
  KC.App (KC.TypeApp (KC.Var tname _) _) args
    | isKokaBuiltinApp (KC.getName tname) -> do
        translateBuiltinApp (KC.getName tname) args

  KC.App (KC.Var tname _) args
    | isKokaBuiltinApp (KC.getName tname) -> do
        translateBuiltinApp (KC.getName tname) args

  -- Cons(h, t) → EApp (EVar kk_cons) [h, t]
  KC.App (KC.TypeApp (KC.Con tname _) _) args
    | isKokaCons (KC.getName tname) -> do
        args' <- mapM translateExpr args
        pure $ F.EApp (F.EVar (F.Name "kk_cons" 0)) args'

  KC.App (KC.Con tname _) args
    | isKokaCons (KC.getName tname) -> do
        args' <- mapM translateExpr args
        pure $ F.EApp (F.EVar (F.Name "kk_cons" 0)) args'

  -- Nil (used as value in a context) → kk_nil()
  KC.App (KC.TypeApp (KC.Con tname _) _) []
    | isKokaNil (KC.getName tname) ->
        pure $ F.EVar (F.Name "kk_nil" 0)
  KC.App (KC.Con tname _) []
    | isKokaNil (KC.getName tname) ->
        pure $ F.EVar (F.Name "kk_nil" 0)

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
    guards'@(_:_:_) -> do
      -- Multiple guards: desugar to nested if-then-else
      body' <- desugarGuards guards'
      pure $ F.Branch pat Nothing body'
    [] ->
      Left "Branch with no guards"
  where
    isExprTrue (KC.Con tname _) = KN.nameStem (KC.getName tname) == "True"
    isExprTrue _ = False

    desugarGuards [] = Left "Branch with no guards"
    desugarGuards [KC.Guard test body] = do
      test' <- translateExpr test
      body' <- translateExpr body
      if isExprTrue test then pure body'
      else pure $ F.ECase test'
        [ F.Branch (F.PatLit (F.LitInt 1)) Nothing body'
        , F.Branch (F.PatWild anyType) Nothing
            (F.ELit (F.LitInt 0))  -- fallthrough: return 0
        ]
    desugarGuards (KC.Guard test body : rest) = do
      test' <- translateExpr test
      body' <- translateExpr body
      rest' <- desugarGuards rest
      if isExprTrue test then pure body'
      else pure $ F.ECase test'
        [ F.Branch (F.PatLit (F.LitInt 1)) Nothing body'
        , F.Branch (F.PatWild anyType) Nothing rest'
        ]

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
    rest' <- mapM translateType rest
    pure (foldl F.TApp (F.TApp t1' t2') rest')

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
  KC.Data dataInfo
    | isEffectDataInfo dataInfo -> pure []  -- handled separately as EffectDecl
    | otherwise -> do
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

-- ============================================================================
-- Effect declaration extraction
-- ============================================================================

-- | Check if a DataInfo represents an effect type
isEffectDataInfo :: KT.DataInfo -> Bool
isEffectDataInfo di = case KT.dataInfoEffect di of
  KS.DataNoEffect -> False
  KS.DataEffect{} -> True

-- | Extract effect declarations from a TypeDefGroup.
--
-- In Koka Core, effect declarations become data types with a special
-- DataEffect marker in dataInfoEffect. Each constructor of the data type
-- represents an effect operation. For example:
--
-- > effect ask<a>
-- >   fun ask() : a
--
-- becomes a data type with one constructor "ask" whose type encodes
-- the operation signature.
extractEffectDecls :: KC.TypeDefGroup -> [F.EffectDecl]
extractEffectDecls (KC.TypeDefGroup tdefs) =
  mapMaybe extractEffectFromTypeDef tdefs

extractEffectFromTypeDef :: KC.TypeDef -> Maybe F.EffectDecl
extractEffectFromTypeDef (KC.Data dataInfo)
  | isEffectDataInfo dataInfo =
      let ops = map conInfoToOpDecl (KT.dataInfoConstrs dataInfo)
      in Just F.EffectDecl
        { F.effectName   = translateQName (KT.dataInfoName dataInfo)
        , F.effectParams = map translateTypeVar (KT.dataInfoParams dataInfo)
        , F.effectOps    = ops
        }
  | otherwise = Nothing
extractEffectFromTypeDef (KC.Synonym _) = Nothing

-- | Translate a constructor of an effect data type to an operation declaration.
--
-- In Koka, effect operations are encoded as constructors where
-- conInfoType gives the full operation type (including foralls and function arrows).
conInfoToOpDecl :: KT.ConInfo -> F.OpDecl
conInfoToOpDecl ci = F.OpDecl
  { F.opName = translateQName (KT.conInfoName ci)
  , F.opType = translateTypeUnsafe (KT.conInfoType ci)
  }

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

-- ============================================================================
-- Koka stdlib builtin recognition
-- ============================================================================

-- | Check if a Koka name is the Nil constructor (std/core/types/Nil)
isKokaNil :: KN.Name -> Bool
isKokaNil kn = KN.nameStem kn == "Nil"
            && KN.nameModule kn `elem` ["std/core/types", "std/core"]

-- | Check if a Koka name is the Cons constructor (std/core/types/Cons)
isKokaCons :: KN.Name -> Bool
isKokaCons kn = KN.nameStem kn == "Cons"
             && KN.nameModule kn `elem` ["std/core/types", "std/core"]

-- | Check if a Koka name corresponds to a known stdlib builtin we
-- want to intercept and translate to a direct runtime call.  Koka's
-- name resolution picks a typed override (`string/println`,
-- `int/println`, etc.) — we use the qualifier to route to the right
-- runtime intrinsic.
isKokaBuiltinApp :: KN.Name -> Bool
isKokaBuiltinApp kn =
  let stem  = KN.nameStem kn
      local = KN.nameLocal kn
      qual  = KN.nameLocalQual kn
  in stem `elem` ["println", "print"]
  || local `elem` ["show/println", "show/print"]
  || stem == "show" && qual `elem` ["show", ""]
  || local == "++"
  || stem == "++"
  || local `elem` builtinIntrinsicNames

-- | Names the bridge wires directly to a runtime call.  Pure-stem or
-- pure-local form, depending on what's stable across Koka name
-- conventions.
builtinIntrinsicNames :: [String]
builtinIntrinsicNames =
  [ "char/string"
  , "chars/count"
  , "range/list"
  , "joinsep/join"
  , "from-int"
  , "tuple2/fst"
  , "tuple2/snd"
  , "foreach"
  ]

-- | Translate a known Koka stdlib application to Frankenstein Core.
-- Koka's println dispatches by type: `string/println`, `int/println`,
-- etc.  We use the qualifier (or local name) to pick the runtime
-- function — strings go straight to `println_str`, ints route through
-- `show_int` first.
translateBuiltinApp :: KN.Name -> [KC.Expr] -> Either Text F.Expr
translateBuiltinApp kn args = do
  let stem  = KN.nameStem kn
      local = KN.nameLocal kn
      qual  = KN.nameLocalQual kn
      printName    = F.Name "println_str" 0
      printNameNoNL = F.Name "print_str" 0
      showIntName  = F.Name "show_int" 0
      strConcatName = F.Name "str_concat" 0
  case (stem, local, qual, args) of
    -- `string/println(s)` — Koka's println for strings: pass the
    -- string directly to the println_str runtime.
    ("println", _, "string", val:_) -> do
      val' <- translateExpr val
      pure $ F.EApp (F.EVar printName) [val']
    ("print", _, "string", val:_) -> do
      val' <- translateExpr val
      pure $ F.EApp (F.EVar printNameNoNL) [val']
    -- `int/println(x)` and the generic `println(x, ?show)` form —
    -- Koka passes the value and an implicit show; we call show_int.
    ("println", _, _, val:_) -> do
      val' <- translateExpr val
      pure $ F.EApp (F.EVar printName) [F.EApp (F.EVar showIntName) [val']]
    (_, "show/println", _, val:_) -> do
      val' <- translateExpr val
      pure $ F.EApp (F.EVar printName) [F.EApp (F.EVar showIntName) [val']]
    -- show(x) → show_int(x)  (loses non-int Show; gap remains)
    ("show", _, _, [val]) -> do
      val' <- translateExpr val
      pure $ F.EApp (F.EVar showIntName) [val']
    -- Koka's binary `++` is string concat at the value level.  The
    -- runtime helper kk_str_concat handles rope-based concatenation.
    (_, "++", _, [a, b]) -> do
      a' <- translateExpr a
      b' <- translateExpr b
      pure $ F.EApp (F.EVar strConcatName) [a', b']
    ("++", _, _, [a, b]) -> do
      a' <- translateExpr a
      b' <- translateExpr b
      pure $ F.EApp (F.EVar strConcatName) [a', b']
    -- Other intrinsics: keep the bare name; the emitter and Linker
    -- recognise them via the runtimeNames Set, so they pass through
    -- to runtime/kk_runtime.c symbols of the same name (after
    -- sanitizeName encodes the `/` as `_`).  Where the runtime
    -- doesn't yet provide an implementation, the link still fails —
    -- the names are at least visible in the Core dump for the
    -- subsequent shim-writing pass.
    (_, l, _, _) | l `elem` builtinIntrinsicNames -> do
      args' <- mapM translateExpr args
      pure $ F.EApp (F.EVar (translateNameK kn)) args'
    -- Fallback: generic translation
    _ -> do
      args' <- mapM translateExpr args
      pure $ F.EApp (F.EVar (translateNameK kn)) args'

