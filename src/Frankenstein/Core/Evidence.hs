-- | Evidence-Passing Translation for Algebraic Effects
--
-- This is the key insight from Koka: algebraic effects can be compiled
-- by passing "evidence" records (structs of operation implementations)
-- as extra function arguments — similar to how typeclass dictionaries
-- are passed in GHC.
--
-- For each @EHandle eff handler body@:
--   1. Build an evidence record containing the handler's operations
--   2. Pass it as an extra argument to the body
--
-- For each @EPerform eff op args@:
--   1. Look up the evidence record in scope
--   2. Extract the operation from the evidence
--   3. Call it with the original args
--
-- After this pass, no EHandle or EPerform nodes remain in the AST.
-- All effect operations have been desugared to plain function calls.

module Frankenstein.Core.Evidence
  ( evidencePass
  , evidencePassDef
  ) where

import Frankenstein.Core.Types

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Evidence scope: maps effect names to evidence variable names
type EvidenceScope = Map Text Name

-- | Run the evidence-passing translation on an entire program.
-- After this pass, EHandle and EPerform are eliminated.
evidencePass :: Program -> Program
evidencePass prog = prog
  { progDefs = map (evidencePassDef (progEffects prog)) (progDefs prog)
  }

-- | Transform a single definition
evidencePassDef :: [EffectDecl] -> Def -> Def
evidencePassDef effs def = def
  { defExpr = evidenceExpr effs Map.empty (defExpr def)
  }

-- | Transform an expression, replacing EHandle/EPerform with plain calls.
--
-- The strategy for EHandle:
--   EHandle effRow handler body
--   =>
--   let ev_exn = handler   -- the handler IS the evidence record
--   in body[perform exn/raise(args) => ev_exn(args)]
--
-- The handler expression is expected to be a lambda that implements
-- the effect operations. For a single-operation effect like exn,
-- the handler itself is the operation implementation.
--
-- For multi-operation effects, the handler would be a record/struct
-- of lambdas (future work).
evidenceExpr :: [EffectDecl] -> EvidenceScope -> Expr -> Expr
evidenceExpr effs scope expr = case expr of
  -- The key transformation: handle -> let-bind evidence, transform body
  EHandle effRow handler body ->
    let effName = effectRowName effRow
        evName  = Name ("ev_" <> effName) 0
        scope'  = Map.insert effName evName scope
        -- Transform the handler (it may itself contain effects)
        handler' = evidenceExpr effs scope handler
        -- Transform the body with the evidence in scope
        body'    = evidenceExpr effs scope' body
        -- Wrap: let ev_<eff> = handler in body
        evBind   = Bind
          { bindName = evName
          , bindType = anyType  -- evidence records are untyped for now
          , bindExpr = handler'
          , bindSort = DefVal
          }
    in ELet [[evBind]] body'

  -- The key transformation: perform -> call through evidence
  EPerform qn args ->
    let effName = qnameModule qn
        opName  = nameText (qnameName qn)
        args'   = map (evidenceExpr effs scope) args
    in case Map.lookup effName scope of
      Just evName ->
        -- Found evidence in scope: call the evidence with args
        -- For single-op effects (like exn/raise), the evidence IS the handler function
        -- For multi-op effects, we'd need to project the right operation
        if isSingleOpEffect effs effName
          then EApp (EVar evName) args'
          else -- Multi-op: evidence is a record, project the operation
               -- For now, use a naming convention: ev_<eff>_<op>
               let projName = Name ("ev_" <> effName <> "_" <> opName) 0
               in EApp (EVar projName) args'
      Nothing ->
        -- No handler in scope — this is an unhandled effect.
        -- Leave as a call to a "default" handler function.
        -- For exn, this becomes a call to kk_exn_raise (runtime abort).
        let defaultFn = Name ("kk_" <> effName <> "_" <> opName) 0
        in EApp (EVar defaultFn) args'

  -- Recurse through all other expression forms
  EVar _     -> expr
  ELit _     -> expr
  ECon _     -> expr

  EApp fn as ->
    EApp (evidenceExpr effs scope fn) (map (evidenceExpr effs scope) as)

  ELam params body ->
    ELam params (evidenceExpr effs scope body)

  ELet bgs body ->
    let bgs'  = map (map (\b -> b { bindExpr = evidenceExpr effs scope (bindExpr b) })) bgs
        body' = evidenceExpr effs scope body
    in ELet bgs' body'

  ECase scrut branches ->
    let scrut'    = evidenceExpr effs scope scrut
        branches' = map (\br -> br { branchBody  = evidenceExpr effs scope (branchBody br)
                                   , branchGuard = fmap (evidenceExpr effs scope) (branchGuard br)
                                   }) branches
    in ECase scrut' branches'

  ETypeApp e ts   -> ETypeApp (evidenceExpr effs scope e) ts
  ETypeLam tvs e  -> ETypeLam tvs (evidenceExpr effs scope e)

  -- Perceus ops: recurse
  ERetain e       -> ERetain  (evidenceExpr effs scope e)
  ERelease e      -> ERelease (evidenceExpr effs scope e)
  EDrop e         -> EDrop    (evidenceExpr effs scope e)
  EReuse e1 e2    -> EReuse   (evidenceExpr effs scope e1) (evidenceExpr effs scope e2)

  -- Laziness: recurse
  EDelay e        -> EDelay (evidenceExpr effs scope e)
  EForce e        -> EForce (evidenceExpr effs scope e)


-- | Extract the primary effect name from an effect row.
-- For EffectRowExtend "exn" _, returns "exn".
effectRowName :: EffectRow -> Text
effectRowName (EffectRowExtend qn _) = qnameModule qn <> nameText (qnameName qn)
effectRowName (EffectRowVar tv)      = nameText (tvName tv)
effectRowName EffectRowEmpty         = "pure"

-- | Check if an effect has exactly one operation (like exn has only "raise").
-- Single-op effects use the handler directly as the evidence;
-- multi-op effects need a record with named projections.
isSingleOpEffect :: [EffectDecl] -> Text -> Bool
isSingleOpEffect effs effName =
  case [ ed | ed <- effs
       , let n = qnameModule (effectName ed) <> nameText (qnameName (effectName ed))
       , n == effName ] of
    [ed] -> length (effectOps ed) <= 1
    _    -> True  -- unknown effect: assume single-op (safe default)

-- | Placeholder type for evidence records
anyType :: Type
anyType = TCon (TypeCon (QName "std" (Name "any" 0)) KindValue)
