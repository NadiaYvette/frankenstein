module StdlibList where

-- Test: Use Haskell's built-in list type instead of custom ADTs.
-- This requires the GHC bridge to correctly translate [] and (:)
-- constructors, and the emitter to handle GHC's list representation.

sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList xs

main :: Int
main = sumList [1, 2, 3, 4, 5]
-- Expected: 15
