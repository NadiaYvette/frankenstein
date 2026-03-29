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

-- | For multi-op effects, maps (effect name, op name) to the bound
-- variable name that holds that specific operation handler.
type OpScope = Map (Text, Text) Name

-- | Combined scope for evidence pass
data Scope = Scope
  { scopeEvidence :: !EvidenceScope
  , scopeOps     :: !OpScope
  } deriving (Show)

emptyScope :: Scope
emptyScope = Scope Map.empty Map.empty

insertEvidence :: Text -> Name -> Scope -> Scope
insertEvidence eff evName s = s { scopeEvidence = Map.insert eff evName (scopeEvidence s) }

insertOp :: Text -> Text -> Name -> Scope -> Scope
insertOp eff op opVarName s = s { scopeOps = Map.insert (eff, op) opVarName (scopeOps s) }

-- | Run the evidence-passing translation on an entire program.
-- After this pass, EHandle and EPerform are eliminated.
evidencePass :: Program -> Program
evidencePass prog = prog
  { progDefs = map (evidencePassDef (progEffects prog)) (progDefs prog)
  }

-- | Transform a single definition
evidencePassDef :: [EffectDecl] -> Def -> Def
evidencePassDef effs def = def
  { defExpr = evidenceExpr effs emptyScope (defExpr def)
  }

-- | Transform an expression, replacing EHandle/EPerform with plain calls.
--
-- The strategy for EHandle:
--   EHandle effRow handler body
--   =>
--   For a single-op effect (like exn with only "raise"):
--     let ev_exn = handler
--     in body[perform exn/raise(args) => ev_exn(args)]
--
--   For a multi-op effect (like console with "println"/"print"):
--     let ev_console = handler
--         ev_console_println = evv_select(ev_console, 0)
--         ev_console_print   = evv_select(ev_console, 1)
--     in body[perform console/println(args) => ev_console_println(args)]
evidenceExpr :: [EffectDecl] -> Scope -> Expr -> Expr
evidenceExpr effs scope expr = case expr of
  -- The key transformation: handle -> let-bind evidence, transform body
  EHandle effRow handler body ->
    let effName = effectRowName effRow
        evName  = Name ("ev_" <> effName) 0
        -- Transform the handler (it may itself contain effects)
        handler' = evidenceExpr effs scope handler
    in case lookupEffectDecl effs effName of
      -- Multi-op effect: bind handler, then project each operation
      Just ed | length (effectOps ed) > 1 ->
        let -- Bind the evidence record
            evBind = Bind
              { bindName = evName
              , bindType = anyType
              , bindExpr = handler'
              , bindSort = DefVal
              }
            -- Create a binding for each operation: ev_<eff>_<op> = evv_select(ev_<eff>, idx)
            opBindsAndNames = zipWith (mkOpBind effName evName) [0..] (effectOps ed)
            opBinds  = map fst opBindsAndNames
            -- Build the new scope with all operation names
            scope' = foldl (\s (opN, varN) -> insertOp effName opN varN s)
                       (insertEvidence effName evName scope)
                       (map snd opBindsAndNames)
            -- Transform the body with the evidence + ops in scope
            body' = evidenceExpr effs scope' body
        in ELet [[evBind] ++ opBinds] body'

      -- Single-op or unknown: the handler IS the evidence (simple case)
      _ ->
        let scope' = insertEvidence effName evName scope
            -- For single-op with a known declaration, also register the operation in OpScope
            scope'' = case lookupEffectDecl effs effName of
              Just ed | [singleOp] <- effectOps ed ->
                let opN = nameText (qnameName (opName singleOp))
                in insertOp effName opN evName scope'
              _ -> scope'
            body' = evidenceExpr effs scope'' body
            evBind = Bind
              { bindName = evName
              , bindType = anyType
              , bindExpr = handler'
              , bindSort = DefVal
              }
        in ELet [[evBind]] body'

  -- The key transformation: perform -> call through evidence
  EPerform qn args ->
    let effName = qnameModule qn
        opN     = nameText (qnameName qn)
        args'   = map (evidenceExpr effs scope) args
    in case Map.lookup (effName, opN) (scopeOps scope) of
      Just opVarName ->
        -- Found a specific operation binding: call it directly
        EApp (EVar opVarName) args'
      Nothing ->
        case Map.lookup effName (scopeEvidence scope) of
          Just evName ->
            -- Evidence in scope but no specific op binding.
            -- For single-op effects, call the evidence directly.
            -- For multi-op effects, project by operation index.
            if isSingleOpEffect effs effName
              then EApp (EVar evName) args'
              else -- Multi-op: project the operation by index
                   let idx = lookupOpIndex effs effName opN
                       projName = Name ("ev_" <> effName <> "_" <> opN) 0
                       -- Generate: let ev_<eff>_<op> = evv_select(ev_<eff>, <idx>)
                       --           in ev_<eff>_<op>(args)
                       selectExpr = EApp (EVar (Name "evv_select" 0))
                                      [ EVar evName
                                      , ELit (LitInt (fromIntegral idx))
                                      ]
                       projBind = Bind
                         { bindName = projName
                         , bindType = anyType
                         , bindExpr = selectExpr
                         , bindSort = DefVal
                         }
                   in ELet [[projBind]] (EApp (EVar projName) args')
          Nothing ->
            -- No handler in scope -- unhandled effect.
            -- Emit a call to a well-known default handler function.
            -- Naming convention: <module>_<effect>_<op>
            let defaultFn = Name (effName <> "_" <> opN) 0
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


-- | Create a binding for a single operation extracted from an evidence record.
-- @ev_<eff>_<op> = evv_select(ev_<eff>, <index>)@
mkOpBind :: Text -> Name -> Int -> OpDecl -> (Bind, (Text, Name))
mkOpBind effName evName idx op =
  let opN     = nameText (qnameName (opName op))
      varName = Name ("ev_" <> effName <> "_" <> opN) 0
      selectExpr = EApp (EVar (Name "evv_select" 0))
                     [ EVar evName
                     , ELit (LitInt (fromIntegral idx))
                     ]
      b = Bind
        { bindName = varName
        , bindType = anyType
        , bindExpr = selectExpr
        , bindSort = DefVal
        }
  in (b, (opN, varName))


-- | Extract the primary effect name from an effect row.
-- For EffectRowExtend "exn" _, returns "exn".
effectRowName :: EffectRow -> Text
effectRowName (EffectRowExtend qn _) = qnameModule qn <> nameText (qnameName qn)
effectRowName (EffectRowVar tv)      = nameText (tvName tv)
effectRowName EffectRowEmpty         = "pure"

-- | Look up an effect declaration by its (flattened) name.
lookupEffectDecl :: [EffectDecl] -> Text -> Maybe EffectDecl
lookupEffectDecl effs effName =
  case [ ed | ed <- effs
       , let n = qnameModule (effectName ed) <> nameText (qnameName (effectName ed))
       , n == effName ] of
    [ed] -> Just ed
    _    -> Nothing

-- | Check if an effect has exactly one operation (like exn has only "raise").
-- Single-op effects use the handler directly as the evidence;
-- multi-op effects need a record with named projections.
-- Unknown effects default to False (multi-op) for safety, so they go
-- through the record-projection path which generates correct dispatch.
isSingleOpEffect :: [EffectDecl] -> Text -> Bool
isSingleOpEffect effs effName =
  case lookupEffectDecl effs effName of
    Just ed -> length (effectOps ed) <= 1
    Nothing -> False  -- unknown effect: assume multi-op (safe default)

-- | Look up the index of an operation within its effect declaration.
-- Returns 0 if the operation or effect is not found (fallback).
lookupOpIndex :: [EffectDecl] -> Text -> Text -> Int
lookupOpIndex effs effName opN =
  case lookupEffectDecl effs effName of
    Just ed -> go 0 (effectOps ed)
    Nothing -> 0
  where
    go _ [] = 0
    go i (op:ops)
      | nameText (qnameName (opName op)) == opN = i
      | otherwise = go (i + 1) ops

-- | Placeholder type for evidence records
anyType :: Type
anyType = TCon (TypeCon (QName "std" (Name "any" 0)) KindValue)
