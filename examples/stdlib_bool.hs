module StdlibBool where

-- Test: Use Haskell's standard Bool type and guards.
-- This requires the GHC bridge to extract the Bool TyCon
-- and correctly handle guard syntax (which GHC desugars to if/case).

abs' :: Int -> Int
abs' n
  | n < 0     = negate n
  | otherwise = n

sign :: Int -> Int
sign n
  | n < 0  = negate 1
  | n == 0 = 0
  | n > 0  = 1
  | otherwise = 0

main :: Int
main = abs' (negate 7) + sign (negate 3) + sign 0 + sign 5
-- Expected: 7 + (-1) + 0 + 1 = 7
