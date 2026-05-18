module ChainedPatBindGuards where

-- Pattern class: function definition using chained `<-` pattern-bind guards.
-- This is the shape `classifyBranches` uses for its SingleConCase arm:
--   | [b] <- conBranches, Nothing <- defaultBranch, ... = SingleConCase ...
--
-- If the self-host pattern-compiler miscompiles chained `<-` guards,
-- this minimal repro should exhibit the divergence.

data MaybeI = NoI | YesI Int

classify :: [MaybeI] -> Maybe MaybeI -> Int
classify ms mDefault
  | [m] <- ms, Nothing <- mDefault, YesI x <- m
  = x
  | otherwise
  = 0

main :: Int
main = classify [YesI 42] Nothing
-- Expect: 42 (all three pattern-bind guards match)
