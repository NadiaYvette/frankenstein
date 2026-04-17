-- | Emit OrganIR (the organ-bank interchange format) from
-- Frankenstein Core IR.
--
-- This is the inverse of 'Frankenstein.OrganIR.Consumer': it converts
-- a Core 'C.Program' back into an 'O.OrganIR' document, which can
-- then be rendered to JSON via 'OrganIR.Json.renderOrganIR'.
--
-- Design decisions (reversing Consumer's lowerings):
--   * Core binary 'TApp' is unfolded back to n-ary OrganIR 'TApp'.
--   * Core named constructor fields are stripped — OrganIR is positional.
--   * Core 'LitChar' is emitted as 'LitInt' (OrganIR has no char literal).
--   * Core 'EFunRef' is emitted as 'EVar' (OrganIR has no func-ref node).
--   * Core 'ERelease' is emitted as 'EDrop' (OrganIR has no release node).
--   * Effect rows are flattened to a list of QNames + optional tail var.
--   * Kind → text: KindValue → omitted, KindEffect → "effect", etc.
--   * DefSort: DefFun → SFun, DefVal → SVal, DefVar → SVal.
--   * Arity is computed from the definition's expression (count of ELam params).

module Frankenstein.OrganIR.Emitter
  ( emitOrganIR
  , emitModule
  , emitProgram
  ) where

import Data.Text (Text)
import Data.Text qualified as T

import OrganIR.Types qualified as O
import OrganIR.Json (renderOrganIR)
import Frankenstein.Core.Types qualified as C

-- * Public API

-- | Convert a Core Program to OrganIR JSON text.
emitProgram :: C.Program -> Text
emitProgram = renderOrganIR . emitOrganIR

-- | Convert a Core Program to an OrganIR document.
emitOrganIR :: C.Program -> O.OrganIR
emitOrganIR prog = O.OrganIR
    { O.irMetadata = defaultMetadata
    , O.irModule   = emitModule prog
    }

-- | Convert a Core Program to an OrganIR Module.
emitModule :: C.Program -> O.Module
emitModule prog = O.Module
    { O.modName       = C.qnameModule (C.progName prog)
    , O.modExports    = []
    , O.modImports    = []
    , O.modDefs       = map emitDef (C.progDefs prog)
    , O.modDataTypes  = map emitDataDecl (C.progData prog)
    , O.modEffectDecls = map emitEffectDecl (C.progEffects prog)
    }

defaultMetadata :: O.Metadata
defaultMetadata = O.Metadata
    { O.metaSourceLang     = O.LHaskell
    , O.metaCompilerVersion = Just "frankenstein"
    , O.metaSourceFile     = Nothing
    , O.metaShimVersion    = O.schemaVersion
    , O.metaTimestamp       = Nothing
    }

-- * Names

emitName :: C.Name -> O.Name
emitName (C.Name t u) = O.Name t u

emitQName :: C.QName -> O.QName
emitQName (C.QName m n) = O.QName m (emitName n)

-- * Visibility

emitVisibility :: C.Visibility -> O.Visibility
emitVisibility C.Public  = O.Public
emitVisibility C.Private = O.Private

-- * Multiplicity

emitMultiplicity :: C.Multiplicity -> O.Multiplicity
emitMultiplicity C.Many   = O.Many
emitMultiplicity C.Affine = O.Affine
emitMultiplicity C.Linear = O.Linear

-- * Kinds

emitKind :: C.Kind -> Maybe Text
emitKind C.KindValue     = Nothing        -- default, omit
emitKind C.KindStar      = Nothing        -- alias for Value
emitKind C.KindEffect    = Just "effect"
emitKind C.KindEffectRow = Just "E"
emitKind C.KindHeap      = Just "heap"
emitKind (C.KindArrow k1 k2) =
    Just $ maybe "V" id (emitKind k1) <> " -> " <> maybe "V" id (emitKind k2)

-- * Types

emitTyVar :: C.TypeVar -> O.TyVar
emitTyVar tv = O.TyVar (emitName (C.tvName tv)) (emitKind (C.tvKind tv))

emitType :: C.Type -> O.Ty
emitType = \case
    C.TForall tvs body ->
        O.TForall (map emitTyVar tvs) (emitType body)

    C.TFun args eff result ->
        O.TFn (map emitFnArg args) (emitEffectRow eff) (emitType result)

    -- Unfold binary TApp back to n-ary: collect TApp spine
    t@(C.TApp _ _) ->
        let (base, args) = collectTApp t
        in case base of
            C.TCon tc -> O.TApp (emitQName (C.tcName tc)) (map emitType args)
            _         -> O.TApp (O.QName "" (O.Name "_app" 0)) (emitType base : map emitType args)

    C.TCon tc -> O.TCon (emitQName (C.tcName tc))

    C.TVar tv -> O.TVar (emitName (C.tvName tv))

    C.TSyn qn _tyArgs expansion ->
        O.TSyn (emitQName qn) (emitType expansion)

-- | Collect a left-nested TApp spine into (base, [args]).
collectTApp :: C.Type -> (C.Type, [C.Type])
collectTApp = go []
  where
    go acc (C.TApp f a) = go (a : acc) f
    go acc t            = (t, acc)

emitFnArg :: (C.Multiplicity, C.Type) -> O.FnArg
emitFnArg (mult, ty) = O.FnArg
    { O.fnArgMultiplicity = case mult of
        C.Many -> Nothing      -- default, omit
        _      -> Just (emitMultiplicity mult)
    , O.fnArgType = emitType ty
    }

-- * Effect rows

emitEffectRow :: C.EffectRow -> O.EffectRow
emitEffectRow row =
    let (effs, tail_) = collectEffectRow row
    in O.EffectRow effs tail_

collectEffectRow :: C.EffectRow -> ([O.QName], Maybe O.Name)
collectEffectRow C.EffectRowEmpty = ([], Nothing)
collectEffectRow (C.EffectRowExtend qn rest) =
    let (effs, tail_) = collectEffectRow rest
    in (emitQName qn : effs, tail_)
collectEffectRow (C.EffectRowVar tv) =
    ([], Just (emitName (C.tvName tv)))

-- * Expressions

emitExpr :: C.Expr -> O.Expr
emitExpr = \case
    C.EVar n        -> O.EVar (emitName n)
    C.ELit lit      -> O.ELit (emitLit lit)
    C.ECon qn       -> O.ECon (emitQName qn) []
    C.EApp f args   -> O.EApp (emitExpr f) (map emitExpr args)
    C.ELam ps body  -> O.ELam (map emitLamParam ps) (emitExpr body)
    C.ELet bgs body -> O.ELet (concatMap (map emitLetBind) bgs) (emitExpr body)
    C.ECase s brs   -> O.ECase (emitExpr s) (map emitBranch brs)
    C.ETypeApp e ts -> O.ETypeApp (emitExpr e) (map emitType ts)
    C.ETypeLam vs e -> O.ETypeLam (map emitTyVar vs) (emitExpr e)

    C.EPerform opQn args ->
        -- Reverse: Core QName → (effect QName, op text)
        -- We use the module as the effect and the name as the op.
        let effQn = O.QName (C.qnameModule opQn) (O.Name "" 0)
            op    = C.nameText (C.qnameName opQn)
        in O.EPerform effQn op (map emitExpr args)

    C.EHandle row handler body ->
        -- Extract the first effect from the row as the OrganIR effect name.
        case row of
            C.EffectRowExtend qn _ -> O.EHandle (emitQName qn) (emitExpr body) (emitExpr handler)
            _ -> O.EHandle (O.QName "" (O.Name "_effect" 0)) (emitExpr body) (emitExpr handler)

    -- Perceus: Core takes Expr, OrganIR takes Name
    C.ERetain (C.EVar n) -> O.ERetain (emitName n)
    C.ERetain e          -> O.ERetain (O.Name (exprToText e) 0)
    C.ERelease (C.EVar n) -> O.EDrop (emitName n)   -- no ERelease in OrganIR
    C.ERelease e          -> O.EDrop (O.Name (exprToText e) 0)
    C.EDrop (C.EVar n)   -> O.EDrop (emitName n)
    C.EDrop e            -> O.EDrop (O.Name (exprToText e) 0)
    C.EReuse (C.EVar n) _ -> O.EReuse (emitName n)
    C.EReuse e _          -> O.EReuse (O.Name (exprToText e) 0)

    C.EDelay e -> O.EDelay (emitExpr e)
    C.EForce e -> O.EForce (emitExpr e)

    C.EFunRef qn -> O.EVar (emitName (C.qnameName qn))

-- | Fallback: render an arbitrary Expr as a name when OrganIR expects Name.
exprToText :: C.Expr -> Text
exprToText (C.EVar n) = C.nameText n
exprToText _          = "_expr"

-- * Literals

emitLit :: C.Lit -> O.Lit
emitLit = \case
    C.LitInt n    -> O.LitInt n
    C.LitFloat d  -> O.LitFloat d
    C.LitString s -> O.LitString s
    C.LitChar c   -> O.LitInt (fromIntegral (fromEnum c))

-- * Lambda parameters

emitLamParam :: (C.Name, C.Type) -> O.LamParam
emitLamParam (n, ty) = O.LamParam
    { O.lpName = emitName n
    , O.lpType = Just (emitType ty)
    }

-- * Let bindings

emitLetBind :: C.Bind -> O.LetBind
emitLetBind b = O.LetBind
    { O.lbName = emitName (C.bindName b)
    , O.lbType = Just (emitType (C.bindType b))
    , O.lbExpr = emitExpr (C.bindExpr b)
    }

-- * Branches and patterns

emitBranch :: C.Branch -> O.Branch
emitBranch br = O.Branch
    { O.brPattern = emitPattern (C.branchPattern br)
    , O.brBody    = case C.branchGuard br of
        Nothing    -> emitExpr (C.branchBody br)
        Just guard -> O.ECase (emitExpr guard)
            [ O.Branch (O.PatLit (O.LitBool True)) (emitExpr (C.branchBody br)) ]
    }

emitPattern :: C.Pattern -> O.Pat
emitPattern = \case
    C.PatCon qn pats ->
        O.PatCon (emitQName qn) (map patToPatBinder pats)
    C.PatLit lit ->
        O.PatLit (emitLit lit)
    C.PatVar n ty ->
        O.PatVar (emitName n) (Just (emitType ty))
    C.PatWild _ty ->
        O.PatWild

-- | Convert a nested Pattern to a PatBinder for constructor args.
patToPatBinder :: C.Pattern -> O.PatBinder
patToPatBinder (C.PatVar n ty) = O.PatBinder (emitName n) (Just (emitType ty))
patToPatBinder (C.PatWild ty)  = O.PatBinder (O.Name "_" 0) (Just (emitType ty))
patToPatBinder _               = O.PatBinder (O.Name "_" 0) Nothing
    -- nested patterns can't be fully represented as PatBinder;
    -- flatten would have already removed deep nesting

-- * Definitions

emitSort :: C.DefSort -> O.Sort
emitSort C.DefFun = O.SFun
emitSort C.DefVal = O.SVal
emitSort C.DefVar = O.SVal

emitDef :: C.Def -> O.Definition
emitDef d = O.Definition
    { O.defName       = emitQName (C.defName d)
    , O.defType       = emitType (C.defType d)
    , O.defExpr       = emitExpr (C.defExpr d)
    , O.defSort       = emitSort (C.defSort d)
    , O.defVisibility = emitVisibility (C.defVisibility d)
    , O.defArity      = computeArity (C.defExpr d)
    }

-- | Compute arity from the outermost ELam.
computeArity :: C.Expr -> Int
computeArity (C.ELam ps _) = length ps
computeArity _              = 0

-- * Data type declarations

emitDataDecl :: C.DataDecl -> O.DataType
emitDataDecl dd = O.DataType
    { O.dtName         = emitQName (C.dataName dd)
    , O.dtTypeParams   = map emitTyVar (C.dataParams dd)
    , O.dtConstructors = map emitConDecl (C.dataCons dd)
    }

emitConDecl :: C.ConDecl -> O.Constructor
emitConDecl cd = O.Constructor
    { O.conName   = emitQName (C.conName cd)
    , O.conFields = map (emitType . snd) (C.conFields cd)  -- strip field names
    }

-- * Effect declarations

emitEffectDecl :: C.EffectDecl -> O.EffectDecl
emitEffectDecl ed = O.EffectDecl
    { O.edName       = emitQName (C.effectName ed)
    , O.edTypeParams = map emitTyVar (C.effectParams ed)
    , O.edOperations = map emitOpDecl (C.effectOps ed)
    }

emitOpDecl :: C.OpDecl -> O.Operation
emitOpDecl op = O.Operation
    { O.opName = C.nameText (C.qnameName (C.opName op))
    , O.opType = emitType (C.opType op)
    }
