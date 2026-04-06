module Arith where

-- | Pure arithmetic — no IO, no typeclasses beyond Num Int.
-- Used for bridge bisimulation testing.
-- Expected result: 2 * (3 + 4) + 5 * 6 = 14 + 30 = 44
compute :: Int -> Int
compute x = x * (3 + 4) + 5 * 6
