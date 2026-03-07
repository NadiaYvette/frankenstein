module Scoring where

-- | Score a pair of integers.
-- Haskell brings: laziness, pattern matching, typeclass-style polymorphism.
score :: Int -> Int -> Int
score x y = x * x + y * y + bonus x + bonus y

bonus :: Int -> Int
bonus n
  | n > 7     = 10
  | n > 4     = 5
  | otherwise = 0
