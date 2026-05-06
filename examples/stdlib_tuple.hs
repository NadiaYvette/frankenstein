module StdlibTuple where

-- Test: Use Haskell's standard tuple type (,).
-- The GHC bridge must extract the (,) TyCon and handle
-- fst/snd which GHC inlines at -O1.

swap :: (Int, Int) -> (Int, Int)
swap (a, b) = (b, a)

addPair :: (Int, Int) -> Int
addPair (a, b) = a + b

main :: Int
main = addPair (swap (10, 3))
-- Expected: 3 + 10 = 13
