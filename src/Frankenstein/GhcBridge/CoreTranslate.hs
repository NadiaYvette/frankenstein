-- | GHC Core -> Frankenstein Core Translation
--
-- Translates GHC's System FC Core into Frankenstein's Core IR.
-- Key mappings:
--   GHC Var        -> EVar
--   GHC App        -> EApp
--   GHC Lam        -> ELam
--   GHC Let        -> ELet
--   GHC Case       -> ECase
--   GHC Cast       -> (type annotation, mostly dropped)
--   GHC Tick       -> (profiling, dropped)
--   GHC Type       -> (type argument, becomes ETypeApp)
--   GHC Coercion   -> (dropped / erased)
--
-- Demand annotations on binders -> Multiplicity + laziness decisions:
--   Strict demand (U) -> value is strict, no EDelay wrapper
--   Lazy demand (L)   -> wrap in EDelay, consumer uses EForce
--   Absent demand (A) -> dead code, drop the binding
--
-- Type class dictionaries -> evidence parameters (mirrors Koka's approach)

module Frankenstein.GhcBridge.CoreTranslate
  ( translateProgram
  , translateExpr
  ) where

import Frankenstein.Core.Types

import Data.Text (Text)
import qualified Data.Text as T

-- | Translate a GHC Core program to Frankenstein Core.
-- Takes the module name and a list of serialized top-level bindings.
translateProgram :: Text -> Text -> Either Text Program
translateProgram modName _coreText =
  -- Phase 1: Parse GHC's textual Core dump
  -- Phase 2: Direct translation from GHC Core AST (when ghc package is linked)
  --
  -- The translation strategy:
  -- 1. Walk each top-level binding group
  -- 2. For recursive groups: emit as BindGroup (mutual recursion)
  -- 3. For non-recursive: emit as singleton BindGroup
  -- 4. Translate types: GHC's ForAllTy -> TForall, FunTy -> TFun
  -- 5. Insert effect annotations: GHC's IO monad -> IO effect row
  -- 6. Dictionary arguments -> evidence parameters
  -- 7. Strictness annotations -> laziness effect decisions
  Left "GHC Core translation not yet implemented"

-- | Translate a single GHC Core expression.
-- Placeholder for when we have direct GHC API access.
translateExpr :: Text -> Either Text Expr
translateExpr _exprText =
  Left "Expression translation not yet implemented"

-- Translation helpers (to be filled in with real GHC Core -> Frankenstein Core):
--
-- translateBind :: GHC.CoreBind -> [BindGroup]
-- translateBind (GHC.NonRec b e) = [[Bind (translateName b) (translateType (GHC.varType b)) (translateExpr' e) (classifyBind b)]]
-- translateBind (GHC.Rec pairs) = [map (\(b,e) -> Bind (translateName b) (translateType (GHC.varType b)) (translateExpr' e) DefFun) pairs]
--
-- translateExpr' :: GHC.CoreExpr -> Expr
-- translateExpr' (GHC.Var v) = EVar (translateName v)
-- translateExpr' (GHC.Lit l) = ELit (translateLit l)
-- translateExpr' (GHC.App f a)
--   | GHC.Type ty <- a = ETypeApp (translateExpr' f) [translateType ty]
--   | otherwise         = EApp (translateExpr' f) [translateExpr' a]
-- translateExpr' (GHC.Lam b e)
--   | GHC.isTyVar b = ETypeLam [translateTyVar b] (translateExpr' e)
--   | otherwise      = ELam [(translateName b, translateType (GHC.varType b))] (translateExpr' e)
-- translateExpr' (GHC.Let bind body) = ELet (translateBind bind) (translateExpr' body)
-- translateExpr' (GHC.Case scrut b _ty alts) = ECase (translateExpr' scrut) (map translateAlt alts)
-- translateExpr' (GHC.Cast e _co) = translateExpr' e  -- drop coercions
-- translateExpr' (GHC.Tick _tick e) = translateExpr' e  -- drop ticks
--
-- classifyBind :: GHC.Id -> DefSort
-- classifyBind b
--   | GHC.isStrictDmd (GHC.idDemandInfo b) = DefVal  -- strict, can be a value
--   | otherwise = DefFun  -- might be a thunk / function
--
-- decideLaziness :: GHC.Id -> GHC.CoreExpr -> Expr
-- decideLaziness b e
--   | GHC.isAbsDmd (GHC.idDemandInfo b) = ELit (LitInt 0)  -- dead code
--   | GHC.isStrictDmd (GHC.idDemandInfo b) = translateExpr' e
--   | otherwise = EDelay (translateExpr' e)  -- lazy: wrap in thunk
