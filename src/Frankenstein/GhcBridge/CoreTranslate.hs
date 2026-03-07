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
  ) where

import qualified Frankenstein.Core.Types as F

import GHC.Core
  ( CoreProgram, CoreBind, CoreExpr
  , Bind(..), Expr(..), Alt(..), AltCon(..)
  )
import GHC.Types.Var (Var, varName, isTyVar)
import GHC.Types.Name (getOccString, nameUnique)
import GHC.Types.Unique (getKey)
import GHC.Types.Literal (Literal(..))
import GHC.Types.Id (idDemandInfo)
import GHC.Types.Demand (isStrictDmd, isAbsDmd)
import GHC.Core.DataCon (dataConName)

import Data.Text (Text)
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Public API
-------------------------------------------------------------------------------

-- | Translate a full GHC Core program to a Frankenstein Program.
translateProgram :: Text -> CoreProgram -> Either Text F.Program
translateProgram modName binds =
  let defs = concatMap translateTopBind binds
  in Right $ F.Program
    { F.progName    = F.QName modName (F.Name modName 0)
    , F.progDefs    = defs
    , F.progData    = []
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
      , F.defType       = anyType
      , F.defExpr       = decideLaziness b e
      , F.defSort       = classifyBind b
      , F.defVisibility = F.Public
      }
  ]
translateTopBind (Rec pairs) =
  [ F.Def
      { F.defName       = qualifyName b
      , F.defType       = anyType
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

-- Type application: App f (Type _) => ETypeApp
trExpr (App f (Type _)) = F.ETypeApp (trExpr f) [anyType]

-- Regular application: collect args into multi-arg EApp
trExpr (App f a) =
  case collectArgs (App f a) of
    (fun, args) -> F.EApp (trExpr fun) (map trExpr args)

-- Type lambda: skip type binders, recurse on body
trExpr (Lam b e)
  | isTyVar b = F.ETypeLam [translateTyVar b] (trExpr e)
  | otherwise  = F.ELam [(translateName b, anyType)] (trExpr e)

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

-------------------------------------------------------------------------------
-- Binding groups
-------------------------------------------------------------------------------

translateBind :: CoreBind -> [F.BindGroup]
translateBind (NonRec b e) =
  [[ F.Bind
      { F.bindName = translateName b
      , F.bindType = anyType
      , F.bindExpr = decideLaziness b e
      , F.bindSort = classifyBind b
      }
  ]]
translateBind (Rec pairs) =
  [[ F.Bind
      { F.bindName = translateName b
      , F.bindType = anyType
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
    (map (\b -> F.PatVar (translateName b) anyType) (filter (not . isTyVar) bndrs))

-------------------------------------------------------------------------------
-- Literals
-------------------------------------------------------------------------------

translateLit :: Literal -> F.Lit
translateLit (LitChar c)       = F.LitChar c
translateLit (LitNumber _ n)   = F.LitInt n
translateLit (LitFloat r)      = F.LitFloat (fromRational r)
translateLit (LitDouble r)     = F.LitFloat (fromRational r)
translateLit (LitString _)     = F.LitString "<bytestring>"
translateLit _                 = F.LitInt 0  -- LitNullAddr, LitRubbish, LitLabel

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

-- | Generic "any" type placeholder.
-- All types map to this until we implement proper type translation.
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
