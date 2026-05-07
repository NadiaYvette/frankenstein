module StdlibHaskell where

-- Haskell library module using REAL Prelude functions on standard types.
-- GHC inlines map/filter/sum at -O1 with aggressive specialization.
-- Called from Koka via extern declarations.

-- Sum of squares of even numbers in a list
sumEvenSquares :: [Int] -> Int
sumEvenSquares xs = sum (map (\x -> x * x) (filter even xs))

-- Build a list [1..n] using standard Prelude
buildRange :: Int -> [Int]
buildRange n = go 1 n
  where
    go lo hi
      | lo > hi   = []
      | otherwise  = lo : go (lo + 1) hi

-- Compose: sum of squares of even numbers in [1..n]
compute :: Int -> Int
compute n = sumEvenSquares (buildRange n)
