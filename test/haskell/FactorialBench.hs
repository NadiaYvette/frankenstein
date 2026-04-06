module FactorialBench where

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Compute factorial(20) which exercises deeper recursion
main :: IO ()
main = print (factorial 20)
