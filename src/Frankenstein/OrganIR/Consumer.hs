-- | Translate OrganIR (the organ-bank interchange format) into
-- Frankenstein Core IR.
--
-- This module is the single entry point for consuming OrganIR JSON
-- that has already been parsed by 'OrganIR.Parse.parseOrganIR'.
--
-- Design gaps (OrganIR features not yet representable in Core):
--   * OrganIR 'ETuple' / 'EList' — lowered to constructor applications
--   * OrganIR 'ERaise' — lowered to EPerform on the "exn" effect
--   * OrganIR 'EUnreachable' — lowered to EApp of a magic "_unreachable" name
--   * OrganIR 'TAny' — mapped to a synthetic TCon "__any"
--   * OrganIR 'TApp' carries a QName + args; Core 'TApp' is binary —
--     we fold left over the arguments.
--   * OrganIR 'SExternal' / 'SCon' sorts have no Core equivalent —
--     mapped to DefFun / DefVal respectively.
--   * OrganIR constructor fields are positional (just types);
--     Core ConDecl expects named fields — we synthesize names "_0", "_1", …
--   * OrganIR kind annotations are optional free-form text;
--     Core kinds are structured — we parse what we can, default to KindValue.
--   * OrganIR patterns use PatBinder (name + optional type);
--     Core PatCon uses nested Pattern — we translate binders to PatVar.
--   * OrganIR let-bindings and lambda params may lack type annotations;
--     we fill in TAny → Core TCon "__any" in those cases.
--   * OrganIR ERetain/ERelease/EDrop/EReuse take a Name;
--     Core equivalents take an Expr — we wrap in EVar.
--   * OrganIR TSyn carries only a QName + expansion (no type args list);
--     Core TSyn wants QName [Type] Type — we pass an empty arg list.

module Frankenstein.OrganIR.Consumer
  ( consumeOrganIR
  , consumeModule
  , consumeProgram
  ) where

import Data.Text (Text)
import Data.Text qualified as T

import OrganIR.Types qualified as O
import OrganIR.Parse qualified as O
import Frankenstein.Core.Types qualified as C

-- * Public API

-- | Translate a parsed OrganIR document into a Frankenstein Core 'C.Program'.
consumeOrganIR :: O.OrganIR -> Either String C.Program
consumeOrganIR oir = consumeModule (O.irModule oir)

-- | Translate an OrganIR module directly.
consumeModule :: O.Module -> Either String C.Program
consumeModule m = do
    defs    <- mapM consumeDef (O.modDefs m)
    datas   <- mapM consumeDataType (O.modDataTypes m)
    effects <- mapM consumeEffectDecl (O.modEffectDecls m)
    let progName = C.QName (O.modName m) (C.Name "module" 0)
    Right C.Program
      { C.progName    = progName
      , C.progDefs    = defs
      , C.progData    = datas
      , C.progEffects = effects
      }

-- | Convenience: parse JSON text and consume in one step.
-- Re-exported so callers don't need to depend on organ-ir directly.
consumeProgram :: Text -> Either String C.Program
consumeProgram txt = case O.parseOrganIR txt of
    Left err  -> Left (T.unpack err)
    Right oir -> consumeOrganIR oir

-- * Names

consumeName :: O.Name -> C.Name
consumeName (O.Name t u) = C.Name t u

consumeQName :: O.QName -> C.QName
consumeQName (O.QName m n) = C.QName m (consumeName n)

-- * Visibility

consumeVisibility :: O.Visibility -> C.Visibility
consumeVisibility O.Public  = C.Public
consumeVisibility O.Private = C.Private

-- * Multiplicity

consumeMultiplicity :: O.Multiplicity -> C.Multiplicity
consumeMultiplicity O.Many   = C.Many
consumeMultiplicity O.Affine = C.Affine
consumeMultiplicity O.Linear = C.Linear

-- | Default multiplicity when OrganIR leaves it unspecified.
defaultMultiplicity :: C.Multiplicity
defaultMultiplicity = C.Many

-- * Kinds

-- | Parse an OrganIR kind string into a Core Kind.
-- OrganIR kinds are optional free-form text; we recognise common ones.
consumeKind :: Maybe Text -> C.Kind
consumeKind Nothing   = C.KindValue
consumeKind (Just k) = case k of
    "V"      -> C.KindValue
    "value"  -> C.KindValue
    "*"      -> C.KindStar
    "X"      -> C.KindEffect
    "effect" -> C.KindEffect
    "E"      -> C.KindEffectRow
    "H"      -> C.KindHeap
    "heap"   -> C.KindHeap
    _        -> C.KindValue  -- fallback

-- * Types

-- | Placeholder type for missing annotations.
anyType :: C.Type
anyType = C.TCon (C.TypeCon (C.QName "" (C.Name "any" 0)) C.KindValue)

consumeTyVar :: O.TyVar -> C.TypeVar
consumeTyVar (O.TyVar n k) = C.TypeVar
    { C.tvName         = consumeName n
    , C.tvKind         = consumeKind k
    , C.tvMultiplicity = C.Many
    }

consumeType :: O.Ty -> C.Type
consumeType = \case
    O.TForall tvs body ->
        C.TForall (map consumeTyVar tvs) (consumeType body)

    O.TFn args eff result ->
        C.TFun (map consumeFnArg args) (consumeEffectRow eff) (consumeType result)

    O.TApp qn tyArgs ->
        -- Core TApp is binary: we build (((TCon qn) `TApp` a1) `TApp` a2) ...
        let base = C.TCon (C.TypeCon (consumeQName qn) C.KindValue)
        in  foldl C.TApp base (map consumeType tyArgs)

    O.TCon qn ->
        C.TCon (C.TypeCon (consumeQName qn) C.KindValue)

    O.TVar n ->
        C.TVar (C.TypeVar (consumeName n) C.KindValue C.Many)

    O.TSyn qn expansion ->
        -- Core TSyn takes QName [Type] Type; OrganIR has no type-arg list.
        C.TSyn (consumeQName qn) [] (consumeType expansion)

    O.TAny -> anyType

consumeFnArg :: O.FnArg -> (C.Multiplicity, C.Type)
consumeFnArg (O.FnArg mMult ty) =
    ( maybe defaultMultiplicity consumeMultiplicity mMult
    , consumeType ty
    )

-- * Effect rows

consumeEffectRow :: O.EffectRow -> C.EffectRow
consumeEffectRow (O.EffectRow effs mTail) =
    let base = case mTail of
            Nothing -> C.EffectRowEmpty
            Just n  -> C.EffectRowVar
                         (C.TypeVar (consumeName n) C.KindEffectRow C.Many)
    in  foldr (C.EffectRowExtend . consumeQName) base effs

-- * Expressions

consumeExpr :: O.Expr -> C.Expr
consumeExpr = \case
    O.EVar n        -> C.EVar (consumeName n)
    O.ELit lit      -> C.ELit (consumeLit lit)
    O.ECon qn args  -> case args of
        [] -> C.ECon (consumeQName qn)
        _  -> C.EApp (C.ECon (consumeQName qn)) (map consumeExpr args)
    O.EApp fn args  -> C.EApp (consumeExpr fn) (map consumeExpr args)
    O.ELam ps body  -> C.ELam (map consumeLamParam ps) (consumeExpr body)
    O.ELet bs body  -> C.ELet [map consumeLetBind bs] (consumeExpr body)
    O.ECase s brs   -> C.ECase (consumeExpr s) (map consumeBranch brs)
    O.ETypeApp e ts -> C.ETypeApp (consumeExpr e) (map consumeType ts)
    O.ETypeLam vs e -> C.ETypeLam (map consumeTyVar vs) (consumeExpr e)

    O.EPerform eff op args ->
        -- Core EPerform takes a QName (the operation) and args.
        -- We synthesize a qualified operation name: <effect_module>.<op>
        let opQName = C.QName (O.qnModule eff) (C.Name op 0)
        in  C.EPerform opQName (map consumeExpr args)

    O.EHandle eff body handler ->
        -- Core EHandle takes EffectRow, handler expr, body expr.
        -- OrganIR gives a single effect QName; we wrap it in a row.
        let row = C.EffectRowExtend (consumeQName eff) C.EffectRowEmpty
        in  C.EHandle row (consumeExpr handler) (consumeExpr body)

    -- Perceus operations: OrganIR takes Name, Core takes Expr
    O.ERetain n  -> C.ERetain (C.EVar (consumeName n))
    O.ERelease n -> C.ERelease (C.EVar (consumeName n))
    O.EDrop n    -> C.EDrop (C.EVar (consumeName n))
    O.EReuse n   -> C.EReuse (C.EVar (consumeName n))
                              (C.EVar (consumeName n))
                            -- GAP: Core EReuse takes two Exprs (alloc, body);
                            -- OrganIR EReuse gives only a name.  We pass the
                            -- same EVar for both arguments as a placeholder.
                            -- TODO: revisit once reuse semantics are clarified.

    -- Laziness
    O.EDelay e -> C.EDelay (consumeExpr e)
    O.EForce e -> C.EForce (consumeExpr e)

    -- Sugar — lower to Core primitives
    O.ETuple es ->
        -- Lower to a constructor application: Tuple#n(e1, …, en)
        let n     = length es
            tName = C.QName "" (C.Name ("Tuple" <> T.pack (show n)) 0)
        in  C.EApp (C.ECon tName) (map consumeExpr es)

    O.EList es ->
        -- Lower to nested Cons/Nil: Cons(e1, Cons(e2, … Nil))
        let nil  = C.ECon (C.QName "" (C.Name "Nil" 0))
            cons = C.QName "" (C.Name "Cons" 0)
        in  foldr (\e acc -> C.EApp (C.ECon cons) [consumeExpr e, acc]) nil es

    O.ERaise e ->
        -- Lower to EPerform on the "exn"/"raise" effect operation
        C.EPerform (C.QName "" (C.Name "raise" 0)) [consumeExpr e]

    O.EUnreachable ->
        -- Lower to application of a magic _unreachable function
        C.EApp (C.EVar (C.Name "_unreachable" 0)) []

-- * Literals

consumeLit :: O.Lit -> C.Lit
consumeLit = \case
    O.LitInt n    -> C.LitInt n
    O.LitFloat d  -> C.LitFloat d
    O.LitString s -> C.LitString s
    O.LitBool b   ->
        -- GAP: Core has no LitBool; lower to LitInt 0/1.
        -- Consumers should check for this convention.
        C.LitInt (if b then 1 else 0)

-- * Lambda parameters

consumeLamParam :: O.LamParam -> (C.Name, C.Type)
consumeLamParam (O.LamParam n mTy) =
    (consumeName n, maybe anyType consumeType mTy)

-- * Let bindings

consumeLetBind :: O.LetBind -> C.Bind
consumeLetBind (O.LetBind n mTy e) = C.Bind
    { C.bindName = consumeName n
    , C.bindType = maybe anyType consumeType mTy
    , C.bindExpr = consumeExpr e
    , C.bindSort = C.DefVal
    }

-- * Branches and patterns

consumeBranch :: O.Branch -> C.Branch
consumeBranch (O.Branch pat body) = C.Branch
    { C.branchPattern = consumePattern pat
    , C.branchGuard   = Nothing   -- OrganIR has no guard expressions
    , C.branchBody    = consumeExpr body
    }

consumePattern :: O.Pat -> C.Pattern
consumePattern = \case
    O.PatCon qn binders ->
        C.PatCon (consumeQName qn) (map consumePatBinder binders)
    O.PatLit lit ->
        C.PatLit (consumeLit lit)
    O.PatVar n mTy ->
        C.PatVar (consumeName n) (maybe anyType consumeType mTy)
    O.PatWild ->
        C.PatWild anyType

consumePatBinder :: O.PatBinder -> C.Pattern
consumePatBinder (O.PatBinder n mTy) =
    C.PatVar (consumeName n) (maybe anyType consumeType mTy)

-- * Definitions

consumeSort :: O.Sort -> C.DefSort
consumeSort = \case
    O.SFun      -> C.DefFun
    O.SVal      -> C.DefVal
    O.SExternal -> C.DefFun  -- GAP: no Core equivalent for external decls
    O.SCon      -> C.DefVal  -- GAP: constructor "defs" mapped to DefVal

consumeDef :: O.Definition -> Either String C.Def
consumeDef d = Right C.Def
    { C.defName       = consumeQName (O.defName d)
    , C.defType       = consumeType (O.defType d)
    , C.defExpr       = consumeExpr (O.defExpr d)
    , C.defSort       = consumeSort (O.defSort d)
    , C.defVisibility = consumeVisibility (O.defVisibility d)
    }

-- * Data type declarations

consumeDataType :: O.DataType -> Either String C.DataDecl
consumeDataType dt = Right C.DataDecl
    { C.dataName   = consumeQName (O.dtName dt)
    , C.dataParams = map consumeTyVar (O.dtTypeParams dt)
    , C.dataCons   = map consumeConstructor (O.dtConstructors dt)
    , C.dataVis    = C.Public  -- OrganIR DataType has no visibility field
    }

consumeConstructor :: O.Constructor -> C.ConDecl
consumeConstructor ctor =
    let fields = zipWith mkField [0 :: Int ..] (O.conFields ctor)
    in  C.ConDecl
          { C.conName   = consumeQName (O.conName ctor)
          , C.conFields = fields
          , C.conVis    = C.Public  -- OrganIR Constructor has no visibility
          }
  where
    mkField i ty = (C.Name ("_" <> T.pack (show i)) 0, consumeType ty)

-- * Effect declarations

consumeEffectDecl :: O.EffectDecl -> Either String C.EffectDecl
consumeEffectDecl ed = Right C.EffectDecl
    { C.effectName   = consumeQName (O.edName ed)
    , C.effectParams = map consumeTyVar (O.edTypeParams ed)
    , C.effectOps    = map consumeOperation (O.edOperations ed)
    }

consumeOperation :: O.Operation -> C.OpDecl
consumeOperation op = C.OpDecl
    { C.opName = C.QName "" (C.Name (O.opName op) 0)
    , C.opType = consumeType (O.opType op)
    }
