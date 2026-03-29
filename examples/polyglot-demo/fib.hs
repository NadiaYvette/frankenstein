module Fib where

-- | Simple recursive fibonacci — pure, no typeclasses, no laziness needed.
-- This is the simplest possible cross-language function: Int -> Int.
fibonacci :: Int -> Int
fibonacci n
  | n <= 0    = 0
  | n == 1    = 1
  | otherwise = fibonacci (n - 1) + fibonacci (n - 2)
