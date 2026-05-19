module ChainedIO where

-- Demonstrates that GHC do-blocks with multiple IO actions sequence
-- correctly through the Frankenstein bridge.
--
-- GHC desugars this to:
--   main$N :: State# RealWorld -> (# State# RealWorld, () #)
--   main$N = \s -> let _ = hPutStr2 stdout "first" True in ...
--   ChainedIO.main = main$N
-- which the bridge intercepts: hPutStr2 → println_haskell_chars, and
-- applyMainIfFunctionAlias rewrites the user's main from `= main$N`
-- to `= main$N(0)` so the IO-action lambda actually runs.

main :: IO ()
main = do
  putStrLn "first"
  putStrLn "second"
  putStrLn "third"
