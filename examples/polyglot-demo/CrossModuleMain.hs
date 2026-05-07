module CrossModuleMain where

-- Haskell module that imports from CrossModuleLib.
-- Demonstrates multi-module Haskell compilation within
-- a cross-language pipeline (Koka calls these functions).

import CrossModuleLib (double, triple, sumList)

-- Combine the library functions into a composite operation.
-- double(n) + triple(n) + sumList([1..n])
analyze :: Int -> Int
analyze n = double n + triple n + sumList (mkRange 1 n)

-- Build a list [lo..hi] without conflicting with Prelude.enumFromTo
mkRange :: Int -> Int -> [Int]
mkRange lo hi
  | lo > hi   = []
  | otherwise = lo : mkRange (lo + 1) hi
