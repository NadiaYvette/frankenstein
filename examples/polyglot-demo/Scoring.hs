module Scoring where

-- | Score a pair of integers.
-- Haskell brings: laziness, pattern matching, guards.
-- In the polyglot pipeline this is called from Koka's main.
score :: Int -> Int -> Int
score x y = x * x + y * y + bonus x + bonus y

bonus :: Int -> Int
bonus n
  | n > 7     = 10
  | n > 4     = 5
  | otherwise = 0
