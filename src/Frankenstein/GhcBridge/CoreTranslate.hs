-- | GHC Core -> Frankenstein Core Translation
--
-- Translates GHC's System FC Core into Frankenstein's Core IR.
-- Key mappings:
--   GHC Var        -> EVar
--   GHC App        -> EApp (collected multi-arg)
--   GHC Lam        -> ELam
--   GHC Let        -> ELet
--   GHC Case       -> ECase
--   GHC Cast       -> (coercion dropped, recurse on inner)
--   GHC Tick       -> (profiling, dropped)
--   GHC Type       -> (type argument, becomes ETypeApp)
--   GHC Coercion   -> (dropped / erased)
--
-- Demand annotations on binders -> laziness decisions:
--   Strict demand  -> no EDelay wrapper
--   Lazy demand    -> wrap in EDelay

module Frankenstein.GhcBridge.CoreTranslate
  ( translateProgram
  , translateExpr
  , translateTyCons
  ) where

import qualified Frankenstein.Core.Types as F

import GHC.Core
  ( CoreProgram, CoreBind, CoreExpr
  , Bind(..), Expr(..), Alt(..), AltCon(..)
  )
import GHC.Core.TyCo.Rep (Type(..), TyLit(..))
import GHC.Core.TyCon
  ( TyCon, tyConName, isAlgTyCon, tyConTyVars, tyConDataCons
  )
import GHC.Types.Var
  ( Var, varName, varType, isTyVar
  , isInvisibleFunArg, binderVar, ForAllTyBinder
  )
import GHC.Types.Name (getOccString, nameUnique, nameModule_maybe)
import GHC.Types.Unique (getKey)
import GHC.Types.Literal (Literal(..))
import GHC.Types.Id (idDemandInfo)
import GHC.Types.Demand (isStrictDmd, isAbsDmd)
import GHC.Core.DataCon (DataCon, dataConName, dataConOrigArgTys, dataConFieldLabels)
import GHC.Unit.Module (moduleNameString, moduleName)
import GHC.Data.FastString (unpackFS)
import GHC.Types.FieldLabel (flLabel)
import GHC.Core.TyCo.Rep (Scaled(..))
import Language.Haskell.Syntax.Basic (FieldLabelString(..))

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)

-------------------------------------------------------------------------------
-- Public API
-------------------------------------------------------------------------------

-- | Translate a full GHC Core program to a Frankenstein Program.
-- Takes the module name, the Core bindings, and the module's TyCons.
translateProgram :: Text -> CoreProgram -> [TyCon] -> Either Text F.Program
translateProgram modName binds tyCons =
  let defs = filter (not . isDictDef) (concatMap translateTopBind binds)
      dataDecls = translateTyCons tyCons
  in Right $ F.Program
    { F.progName    = F.QName modName (F.Name modName 0)
    , F.progDefs    = defs
    , F.progData    = dataDecls
    , F.progEffects = []
    }

-- | Translate a single GHC Core expression (public entry point).
translateExpr :: CoreExpr -> F.Expr
translateExpr = trExpr

-------------------------------------------------------------------------------
-- Top-level bindings -> Defs
-------------------------------------------------------------------------------

translateTopBind :: CoreBind -> [F.Def]
translateTopBind (NonRec b e) =
  [ F.Def
      { F.defName       = qualifyName b
      , F.defType       = translateType (varType b)
      , F.defExpr       = decideLaziness b e
      , F.defSort       = classifyBind b
      , F.defVisibility = F.Public
      }
  ]
translateTopBind (Rec pairs) =
  [ F.Def
      { F.defName       = qualifyName b
      , F.defType       = translateType (varType b)
      , F.defExpr       = trExpr e
      , F.defSort       = F.DefFun
      , F.defVisibility = F.Public
      }
  | (b, e) <- pairs
  ]

-------------------------------------------------------------------------------
-- Expression translation
-------------------------------------------------------------------------------

trExpr :: CoreExpr -> F.Expr
trExpr (Var v) = F.EVar (translateName v)

trExpr (Lit l) = F.ELit (translateLit l)

-- Type application: App f (Type t) => ETypeApp
trExpr (App f (Type t)) = F.ETypeApp (trExpr f) [translateType t]

-- Detect unpackCString# applied to a string literal: extract the string directly
trExpr (App (Var v) (Lit (LitString bs)))
  | getOccString v `elem` ["unpackCString#", "unpackCStringUtf8#"] =
      F.ELit (F.LitString (TE.decodeUtf8With lenientDecode bs))

-- Regular application: collect args, strip dictionaries, simplify I#
trExpr (App f a) =
  case collectArgs (App f a) of
    -- I#(literal) → just the literal (unbox)
    (Var v, [Lit l])
      | getOccString v == "I#" -> F.ELit (translateLit l)
    -- General case: strip dictionary arguments
    (fun, args) ->
      let args' = filter (not . isDictArg) args
      in F.EApp (trExpr fun) (map trExpr args')

-- Type lambda: skip type binders, recurse on body
trExpr (Lam b e)
  | isTyVar b = F.ETypeLam [translateTyVar b] (trExpr e)
  | otherwise  = F.ELam [(translateName b, translateType (varType b))] (trExpr e)

-- Let: translate binding groups
trExpr (Let bind body) =
  F.ELet (translateBind bind) (trExpr body)

-- Case: translate scrutinee and alternatives
trExpr (Case scrut _bndr _ty alts) =
  F.ECase (trExpr scrut) (map translateAlt alts)

-- Cast: drop coercion, recurse
trExpr (Cast e _co) = trExpr e

-- Tick: drop profiling tick, recurse
trExpr (Tick _tick e) = trExpr e

-- Type: skip (shouldn't appear at expression level normally)
trExpr (Type _) = F.ELit (F.LitInt 0)

-- Coercion: skip
trExpr (Coercion _) = F.ELit (F.LitInt 0)

-------------------------------------------------------------------------------
-- Collect nested applications into (fun, [args])
-- Skips type arguments along the way
-------------------------------------------------------------------------------

collectArgs :: CoreExpr -> (CoreExpr, [CoreExpr])
collectArgs = go []
  where
    go args (App f (Type _)) = go args f            -- skip type args
    go args (App f a)        = go (a : args) f       -- collect value args
    go args e                = (e, args)

-- | Is this expression a typeclass dictionary argument?
-- GHC desugars typeclasses to dictionary-passing: e.g. (<=) $fOrdInt x y
-- We strip these since Frankenstein uses direct builtin operations.
isDictArg :: CoreExpr -> Bool
isDictArg (Var v) =
  let name = getOccString v
  in isPrefixOf "$f" name     -- $fOrdInt, $fNumInt, $fEqInt, etc.
     || isPrefixOf "$d" name  -- $dOrd, $dNum, etc. (alternative naming)
     || isPrefixOf "$c" name  -- $c==, etc. (method selectors)
     || isPrefixOf "$W" name  -- $WI# etc. (wrappers)
isDictArg _ = False

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- | Is this a compiler-generated dictionary/module definition?
-- These are GHC's desugared typeclass infrastructure and module metadata.
isDictDef :: F.Def -> Bool
isDictDef d =
  let n = T.unpack (F.nameText (F.qnameName (F.defName d)))
  in isPrefixOf "$f" n || isPrefixOf "$d" n || isPrefixOf "$c" n
     || isPrefixOf "$tr" n || isPrefixOf "$W" n || isPrefixOf "$tc" n

-------------------------------------------------------------------------------
-- Binding groups
-------------------------------------------------------------------------------

translateBind :: CoreBind -> [F.BindGroup]
translateBind (NonRec b e) =
  [[ F.Bind
      { F.bindName = translateName b
      , F.bindType = translateType (varType b)
      , F.bindExpr = decideLaziness b e
      , F.bindSort = classifyBind b
      }
  ]]
translateBind (Rec pairs) =
  [[ F.Bind
      { F.bindName = translateName b
      , F.bindType = translateType (varType b)
      , F.bindExpr = trExpr e
      , F.bindSort = F.DefFun
      }
   | (b, e) <- pairs
  ]]

-------------------------------------------------------------------------------
-- Case alternatives
-------------------------------------------------------------------------------

translateAlt :: Alt Var -> F.Branch
translateAlt (Alt con bndrs rhs) = F.Branch
  { F.branchPattern = translateAltCon con bndrs
  , F.branchGuard   = Nothing
  , F.branchBody    = trExpr rhs
  }

translateAltCon :: AltCon -> [Var] -> F.Pattern
translateAltCon DEFAULT _ = F.PatWild anyType
translateAltCon (LitAlt lit) _ = F.PatLit (translateLit lit)
translateAltCon (DataAlt dc) bndrs =
  F.PatCon
    (F.QName T.empty (F.Name (T.pack (getOccString (dataConName dc))) 0))
    (map (\b -> F.PatVar (translateName b) (translateType (varType b))) (filter (not . isTyVar) bndrs))

-------------------------------------------------------------------------------
-- Literals
-------------------------------------------------------------------------------

translateLit :: Literal -> F.Lit
translateLit (LitChar c)       = F.LitChar c
translateLit (LitNumber _ n)   = F.LitInt n
translateLit (LitFloat r)      = F.LitFloat (fromRational r)
translateLit (LitDouble r)     = F.LitFloat (fromRational r)
translateLit (LitString bs)    = F.LitString (TE.decodeUtf8With lenientDecode bs)
translateLit (LitLabel fs _)   = F.LitString (T.pack (unpackFS fs))
translateLit LitNullAddr       = F.LitInt 0  -- null pointer
translateLit (LitRubbish _ _)  = F.LitInt 0  -- undefined/bottom placeholder

-------------------------------------------------------------------------------
-- Names and type helpers
-------------------------------------------------------------------------------

translateName :: Var -> F.Name
translateName v = F.Name
  { F.nameText   = T.pack (getOccString v)
  , F.nameUnique = fromIntegral (getKey (nameUnique (varName v)))
  }

translateTyVar :: Var -> F.TypeVar
translateTyVar v = F.TypeVar
  { F.tvName         = translateName v
  , F.tvKind         = F.KindStar
  , F.tvMultiplicity = F.Many
  }

qualifyName :: Var -> F.QName
qualifyName v = F.QName T.empty (translateName v)

-------------------------------------------------------------------------------
-- Type translation
-------------------------------------------------------------------------------

-- | Translate a GHC Type to Frankenstein Core Type.
translateType :: Type -> F.Type

-- Function type: skip invisible (dictionary/constraint) arguments
translateType (FunTy flag _mult arg res)
  | isInvisibleFunArg flag = translateType res
  | otherwise =
      case translateType res of
        -- Accumulate consecutive function args into a single TFun
        F.TFun args eff retTy ->
          F.TFun ((F.Many, translateType arg) : args) eff retTy
        resTy ->
          F.TFun [(F.Many, translateType arg)] F.EffectRowEmpty resTy

-- Type constructor with no arguments
translateType (TyConApp tc []) =
  F.TCon $ F.TypeCon
    { F.tcName = tyConQName tc
    , F.tcKind = F.KindStar
    }

-- Type constructor applied to arguments
translateType (TyConApp tc args) =
  foldl F.TApp
    (F.TCon $ F.TypeCon { F.tcName = tyConQName tc, F.tcKind = F.KindStar })
    (map translateType args)

-- Type variable
translateType (TyVarTy v) =
  F.TVar $ F.TypeVar
    { F.tvName         = translateName v
    , F.tvKind         = F.KindStar
    , F.tvMultiplicity = F.Many
    }

-- Forall: collect consecutive foralls into one TForall
translateType (ForAllTy bndr ty) =
  let (bndrs, innerTy) = collectForAlls ty
      allBndrs = bndr : bndrs
      tvs = map forAllBndrToTypeVar allBndrs
  in F.TForall tvs (translateType innerTy)

-- Type application
translateType (AppTy f a) =
  F.TApp (translateType f) (translateType a)

-- Literal types: use string representation as a TCon
translateType (LitTy (NumTyLit n)) =
  F.TCon $ F.TypeCon
    { F.tcName = F.QName T.empty (F.Name (T.pack (show n)) 0)
    , F.tcKind = F.KindStar
    }
translateType (LitTy (StrTyLit fs)) =
  F.TCon $ F.TypeCon
    { F.tcName = F.QName T.empty (F.Name (T.pack (unpackFS fs)) 0)
    , F.tcKind = F.KindStar
    }
translateType (LitTy (CharTyLit c)) =
  F.TCon $ F.TypeCon
    { F.tcName = F.QName T.empty (F.Name (T.pack [c]) 0)
    , F.tcKind = F.KindStar
    }

-- Cast: ignore coercion, recurse on inner type
translateType (CastTy ty _) = translateType ty

-- Coercion type: fallback to anyType
translateType (CoercionTy _) = anyType

-- | Collect consecutive ForAllTy binders.
collectForAlls :: Type -> ([ForAllTyBinder], Type)
collectForAlls (ForAllTy b t) =
  let (bs, inner) = collectForAlls t
  in (b : bs, inner)
collectForAlls t = ([], t)

-- | Convert a ForAllTyBinder to a Frankenstein TypeVar.
forAllBndrToTypeVar :: ForAllTyBinder -> F.TypeVar
forAllBndrToTypeVar bndr =
  let tv = binderVar bndr
  in F.TypeVar
    { F.tvName         = translateName tv
    , F.tvKind         = F.KindStar
    , F.tvMultiplicity = F.Many
    }

-- | Build a QName from a GHC TyCon.
tyConQName :: TyCon -> F.QName
tyConQName tc =
  let n = tyConName tc
      nameT = T.pack (getOccString n)
      modPfx = case nameModule_maybe n of
                 Just m  -> T.pack (moduleNameString (moduleName m))
                 Nothing -> ""
  in F.QName modPfx (F.Name nameT 0)

-- | Generic "any" type placeholder (fallback for CoercionTy).
anyType :: F.Type
anyType = F.TCon $ F.TypeCon
  { F.tcName = F.QName T.empty (F.Name "any" 0)
  , F.tcKind = F.KindValue
  }

-------------------------------------------------------------------------------
-- Demand / laziness classification
-------------------------------------------------------------------------------

-- | Classify a binding based on GHC's demand information.
classifyBind :: Var -> F.DefSort
classifyBind b
  | isStrictDmd (idDemandInfo b) = F.DefVal
  | otherwise                     = F.DefFun

-- | Decide whether to wrap an expression in EDelay based on demand.
-- Strict bindings: use expression directly.
-- Absent bindings: dead code, emit a placeholder.
-- Lazy bindings: wrap in EDelay (thunk).
decideLaziness :: Var -> CoreExpr -> F.Expr
decideLaziness b e
  | isAbsDmd (idDemandInfo b)    = F.ELit (F.LitInt 0)  -- dead code
  | isStrictDmd (idDemandInfo b) = trExpr e              -- strict: no thunk
  | otherwise                     = F.EDelay (trExpr e)  -- lazy: wrap in thunk

-------------------------------------------------------------------------------
-- TyCon -> DataDecl translation
-------------------------------------------------------------------------------

-- | Translate a list of GHC TyCons to Frankenstein DataDecls.
-- Only algebraic TyCons (data/newtype) are included; type synonyms,
-- type families, primitive TyCons, etc. are filtered out.
translateTyCons :: [TyCon] -> [F.DataDecl]
translateTyCons = map translateTyCon . filter isAlgTyCon

-- | Translate a single algebraic TyCon to a DataDecl.
translateTyCon :: TyCon -> F.DataDecl
translateTyCon tc = F.DataDecl
  { F.dataName   = tyConQName tc
  , F.dataParams = map translateTyVar (tyConTyVars tc)
  , F.dataCons   = map translateDataCon (tyConDataCons tc)
  , F.dataVis    = F.Public
  }

-- | Translate a GHC DataCon to a Frankenstein ConDecl.
translateDataCon :: DataCon -> F.ConDecl
translateDataCon dc =
  let dcName = F.QName T.empty (F.Name (T.pack (getOccString (dataConName dc))) 0)
      -- Field labels: if the constructor has record fields, use those names;
      -- otherwise generate positional names (field_0, field_1, ...)
      labels = dataConFieldLabels dc
      argTys = dataConOrigArgTys dc
      fields
        | null labels =
            [ (F.Name ("field_" <> T.pack (show i)) 0, translateType ty)
            | (i, Scaled _ ty) <- zip [(0 :: Int)..] argTys
            ]
        | otherwise =
            [ (F.Name (T.pack (unpackFS (field_label (flLabel fl)))) 0, translateType ty)
            | (fl, Scaled _ ty) <- zip labels argTys
            ]
  in F.ConDecl
    { F.conName   = dcName
    , F.conFields = fields
    , F.conVis    = F.Public
    }
