{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
-- | Module: Frankenstein.Opt.EGraph
-- Description: Verification-guided algebraic rewrite and equality-saturation optimizer for OrganIR
--
-- Optimizes @OrganIR@ using sound Perceus RC laws, @Polytropos@/@Penelope@
-- @RowMorph@ frame fusion, immediate first-order thunk strictness promotion,
-- and Liquid-witnessed bounds-check elimination under a lexicographic cost model.
module Frankenstein.Opt.EGraph
  ( OptExpr(..)
  , OptCost(..)
  , RewriteStats(..)
  , composePerm
  , isIdentityPerm
  , exprCost
  , rewriteOnce
  , rewriteStep
  , saturateOrganExpr
  ) where

import Data.Text (Text)

-- | Normalized OrganIR expression kernel targeted by Track 1 algebraic rewrites.
data OptExpr
  = EVar !Text
  | EInt !Integer
  | ESeq !OptExpr !OptExpr
  | ENop
  | EDup !OptExpr
  | EDrop !OptExpr
  | EAllocCtor !Text ![OptExpr]
  | EInplaceReuse !Text !Text ![OptExpr]
    -- ^ @(reuseVar, ctorTag, fields)@
  | EAllocThunk !OptExpr
  | EForce !OptExpr
  | ERowMorph ![Int] !OptExpr
    -- ^ Frame slot permutation / projection over a Polytropos/Penelope register frame.
  | ECheckedIndex !OptExpr !OptExpr !Bool
    -- ^ @(arr, idx, liquidProvenInBounds)@
  | EUncheckedIndex !OptExpr !OptExpr
  deriving stock (Eq, Ord, Show)

-- | Lexicographic extraction cost prioritizing allocation and RC elimination.
data OptCost = OptCost
  { ocHeapAllocs  :: {-# UNPACK #-} !Int
  , ocThunkAllocs :: {-# UNPACK #-} !Int
  , ocRcOps       :: {-# UNPACK #-} !Int
  , ocBoundsChks  :: {-# UNPACK #-} !Int
  , ocInstrs      :: {-# UNPACK #-} !Int
  } deriving stock (Eq, Ord, Show)

instance Semigroup OptCost where
  OptCost h1 t1 r1 b1 i1 <> OptCost h2 t2 r2 b2 i2 =
    OptCost (h1 + h2) (t1 + t2) (r1 + r2) (b1 + b2) (i1 + i2)

instance Monoid OptCost where
  mempty = OptCost 0 0 0 0 0

-- | Telemetry counters emitted by 'saturateOrganExpr'.
data RewriteStats = RewriteStats
  { rsIterations :: !Int
  , rsCostBefore :: !OptCost
  , rsCostAfter  :: !OptCost
  } deriving stock (Eq, Show)

-- | Composes two finite slot-index permutations @(outer . inner)@.
composePerm :: [Int] -> [Int] -> [Int]
composePerm outer inner =
  [ if i >= 0 && i < length inner then inner !! i else i
  | i <- outer
  ]

-- | Returns 'True' iff a permutation is strictly the identity @[0, 1, ..., n-1]@.
isIdentityPerm :: [Int] -> Bool
isIdentityPerm xs = xs == [0 .. length xs - 1]

-- | Computes the static 'OptCost' of an 'OptExpr'.
exprCost :: OptExpr -> OptCost
exprCost ENop                    = mempty
exprCost (EVar _)                = OptCost 0 0 0 0 1
exprCost (EInt _)                = OptCost 0 0 0 0 1
exprCost (ESeq a b)              = exprCost a <> exprCost b
exprCost (EDup e)                = OptCost 0 0 1 0 1 <> exprCost e
exprCost (EDrop e)               = OptCost 0 0 1 0 1 <> exprCost e
exprCost (EAllocCtor _ args)     = OptCost 1 0 0 0 2 <> foldMap exprCost args
exprCost (EInplaceReuse _ _ a)   = OptCost 0 0 0 0 2 <> foldMap exprCost a
exprCost (EAllocThunk e)         = OptCost 1 1 0 0 2 <> exprCost e
exprCost (EForce e)              = OptCost 0 0 0 0 2 <> exprCost e
exprCost (ERowMorph p e)         = OptCost 1 0 0 0 (1 + length p) <> exprCost e
exprCost (ECheckedIndex a i _)   = OptCost 0 0 0 1 3 <> exprCost a <> exprCost i
exprCost (EUncheckedIndex a i)   = OptCost 0 0 0 0 1 <> exprCost a <> exprCost i

-- | Single bottom-up pass applying verified OrganIR algebraic laws.
rewriteOnce :: OptExpr -> OptExpr
rewriteOnce expr =
  case recurse expr of
    -- Law 1: Perceus dup/drop cancellation
    EDrop (EDup _)                    -> ENop
    -- Law 2: Sequencing with ENop
    ESeq ENop b                       -> b
    ESeq a ENop                       -> a
    -- Law 3: Immediate force of freshly allocated first-order thunk -> strict eval
    EForce (EAllocThunk body)         -> body
    -- Law 4: Polytropos/Penelope RowMorph fusion & identity deforestation
    ERowMorph p e | isIdentityPerm p  -> e
    ERowMorph p2 (ERowMorph p1 e)     ->
      let p12 = composePerm p2 p1
      in if isIdentityPerm p12 then e else ERowMorph p12 e
    -- Law 5: Liquid-witnessed bounds-check elimination
    ECheckedIndex a i True            -> EUncheckedIndex a i
    other                             -> other
  where
    recurse (ESeq a b)            = ESeq (rewriteOnce a) (rewriteOnce b)
    recurse (EDup e)              = EDup (rewriteOnce e)
    recurse (EDrop e)             = EDrop (rewriteOnce e)
    recurse (EAllocCtor t args)   = EAllocCtor t (map rewriteOnce args)
    recurse (EInplaceReuse v t a) = EInplaceReuse v t (map rewriteOnce a)
    recurse (EAllocThunk e)       = EAllocThunk (rewriteOnce e)
    recurse (EForce e)            = EForce (rewriteOnce e)
    recurse (ERowMorph p e)       = ERowMorph p (rewriteOnce e)
    recurse (ECheckedIndex a i p) = ECheckedIndex (rewriteOnce a) (rewriteOnce i) p
    recurse (EUncheckedIndex a i) = EUncheckedIndex (rewriteOnce a) (rewriteOnce i)
    recurse leaf                  = leaf

-- | Synonym for 'rewriteOnce' conforming to task packet codeBinding targetSymbols.
rewriteStep :: OptExpr -> OptExpr
rewriteStep = rewriteOnce

-- | Iterates rewriting to a fixed point within @maxSteps@ fuel, returning telemetry.
saturateOrganExpr :: Int -> OptExpr -> (OptExpr, RewriteStats)
saturateOrganExpr maxSteps e0 =
  let c0 = exprCost e0
  in go 0 e0 c0
  where
    go !n !cur !c0
      | n >= maxSteps = (cur, RewriteStats n c0 (exprCost cur))
      | otherwise =
          let next = rewriteOnce cur
          in if next == cur
               then (cur, RewriteStats n c0 (exprCost cur))
               else go (n + 1) next c0
