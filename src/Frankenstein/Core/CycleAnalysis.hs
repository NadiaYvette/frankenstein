-- | Static Cycle Analysis for Perceus RC
--
-- Detects potential cycle sources in Frankenstein Core IR. A cycle can
-- occur when a recursive binding constructs data that contains a reference
-- back to itself (directly or transitively).
--
-- Examples of cyclic patterns:
--   let rec xs = Cons 1 xs          -- direct self-reference in constructor
--   let rec f = \x -> g x           -- mutual recursion through closures
--          g = \x -> f x
--
-- Examples of non-cyclic patterns:
--   let rec fac = \n -> ... fac ... -- recursive function (closure, not data cycle)
--   let xs = Cons 1 (Cons 2 Nil)   -- finite data, no back-edge
--
-- The analysis is conservative: it may report false positives (marking
-- acyclic patterns as potentially cyclic) but never false negatives.
--
-- Output: for each definition, whether it is potentially cyclic. This
-- information can be used by the MLIR emitter to decide whether to
-- register objects with the cycle collector.

module Frankenstein.Core.CycleAnalysis
  ( CycleInfo(..)
  , analyzeCycles
  , isPotentiallyCyclic
  ) where

import Frankenstein.Core.Types

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- | Cycle analysis result for a single definition
data CycleInfo = CycleInfo
  { ciName     :: !QName
  , ciCyclic   :: !Bool    -- ^ Could this def participate in a cycle?
  , ciReason   :: !Text    -- ^ Human-readable reason (for diagnostics)
  } deriving (Show, Eq)

-- | Analyze all definitions in a program for potential cycles.
analyzeCycles :: Program -> [CycleInfo]
analyzeCycles prog = map analyzeDef (progDefs prog)

-- | Is a specific definition potentially cyclic?
isPotentiallyCyclic :: Program -> QName -> Bool
isPotentiallyCyclic prog qn =
  any (\ci -> ciName ci == qn && ciCyclic ci) (analyzeCycles prog)

-- | Analyze a single definition for cycle potential.
analyzeDef :: Def -> CycleInfo
analyzeDef def =
  let name = defName def
      expr = defExpr def
      defNameText = nameText (qnameName name)
      -- Check: does this def's body contain a constructor that captures
      -- the def's own name? That would create a cyclic data structure.
      selfRefs = findSelfRefsInData defNameText expr
  in CycleInfo
    { ciName   = name
    , ciCyclic = not (Set.null selfRefs)
    , ciReason = if Set.null selfRefs
                 then "acyclic"
                 else "self-reference in constructor: " <> head (Set.toList selfRefs)
    }

-- | Find constructor applications in the expression that capture a name
-- matching the definition being analyzed. This indicates a potential cycle:
-- the constructed data will contain a pointer back to the enclosing binding.
--
-- We look for patterns like:
--   ECon qn applied to args that include (EVar defName)
--   EApp (ECon qn) args that include (EVar defName)
--
-- We do NOT flag:
--   EApp (EVar defName) args  -- this is a function CALL, not a data cycle
--   ELam ... (EVar defName)   -- this is a closure capturing defName (function, not data)
findSelfRefsInData :: Text -> Expr -> Set Text
findSelfRefsInData defName expr = go expr
  where
    go (EVar _)         = Set.empty
    go (ELit _)         = Set.empty
    go (ECon _)         = Set.empty
    go (EFunRef _)      = Set.empty

    -- Constructor application: check if any argument references defName
    go (EApp (ECon _qn) args) =
      let argRefs = concatMap (collectVarRefs) args
          selfArgs = filter (\n -> nameText n == defName) argRefs
      in if null selfArgs
         then Set.unions (map go args)  -- recurse into args
         else Set.singleton ("data constructor captures '" <> defName <> "'")

    -- Regular application: recurse into subexpressions
    -- Note: EApp (EVar f) args where f == defName is a recursive CALL,
    -- not a data cycle. We don't flag it.
    go (EApp f args)    = Set.unions (go f : map go args)

    -- Lambda: the body might contain data cycles, but the lambda itself
    -- is a function value, not cyclic data.
    go (ELam _ body)    = go body

    -- Let: check both bindings and body. Recursive bindings within
    -- a let could create cycles too.
    -- Explicit concatMap form (see commit a5a578c).
    go (ELet bgs body)  =
      let bgRefs = Set.unions (concatMap (map (go . bindExpr)) bgs)
          -- Also check for mutual recursion creating data cycles
          bgNames = Set.fromList (concatMap (map (nameText . bindName)) bgs)
          mutualCycles = Set.unions
            (concatMap (map (findMutualDataCycles bgNames . bindExpr)) bgs)
      in Set.unions [bgRefs, mutualCycles, go body]

    -- Case: recurse into scrutinee and branches
    go (ECase scrut branches) =
      Set.unions (go scrut : [go (branchBody br) | br <- branches])

    -- Type-level: pass through
    go (ETypeApp e _)   = go e
    go (ETypeLam _ e)   = go e

    -- Effects: recurse
    go (EPerform _ args) = Set.unions (map go args)
    go (EHandle _ h b)   = Set.union (go h) (go b)

    -- Perceus ops: recurse
    go (ERetain e)       = go e
    go (ERelease e)      = go e
    go (EDrop e)         = go e
    go (EReuse _ e)      = go e

    -- Laziness: recurse (thunks can create cycles!)
    go (EDelay e)        = go e
    go (EForce e)        = go e

-- | Collect all variable references in an expression (flat, not recursive)
collectVarRefs :: Expr -> [Name]
collectVarRefs (EVar n)        = [n]
collectVarRefs (EApp f args)   = collectVarRefs f ++ concatMap collectVarRefs args
collectVarRefs (ELet bgs body) = concatMap (\bg -> concatMap (collectVarRefs . bindExpr) bg) bgs ++ collectVarRefs body
collectVarRefs (ECase s brs)   = collectVarRefs s ++ concatMap (collectVarRefs . branchBody) brs
collectVarRefs (EDelay e)      = collectVarRefs e
collectVarRefs (EForce e)      = collectVarRefs e
collectVarRefs (ERetain e)     = collectVarRefs e
collectVarRefs (EDrop e)       = collectVarRefs e
collectVarRefs _               = []

-- | Find mutual data cycles within a set of let-bound names.
-- If binding A constructs data containing B, and B constructs data
-- containing A, that's a mutual cycle.
findMutualDataCycles :: Set Text -> Expr -> Set Text
findMutualDataCycles _bgNames (EApp (ECon _) args) =
  -- Check if constructor args reference any of the let-bound names
  let refs = concatMap collectVarRefs args
      mutualRefs = filter (\n -> Set.member (nameText n) _bgNames) refs
  in if null mutualRefs
     then Set.empty
     else Set.singleton "mutual data cycle in let-rec"
findMutualDataCycles bgNames (ELet bgs body) =
  -- Explicit concatMap form (see commit a5a578c).
  Set.unions (findMutualDataCycles bgNames body :
    concatMap (map (findMutualDataCycles bgNames . bindExpr)) bgs)
findMutualDataCycles bgNames (ECase s brs) =
  Set.unions (findMutualDataCycles bgNames s :
    [findMutualDataCycles bgNames (branchBody br) | br <- brs])
findMutualDataCycles bgNames (EApp f args) =
  Set.unions (findMutualDataCycles bgNames f :
    map (findMutualDataCycles bgNames) args)
findMutualDataCycles bgNames (ELam _ body) = findMutualDataCycles bgNames body
findMutualDataCycles bgNames (EDelay e) = findMutualDataCycles bgNames e
findMutualDataCycles _ _ = Set.empty
