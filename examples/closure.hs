module Closure where

-- Closure: a function that captures its environment.

apply :: (Int -> Int) -> Int -> Int
apply f x = f x

addN :: Int -> Int -> Int
addN n x = n + x

main :: Int
main = apply (addN 10) 32
-- addN 10 creates a closure capturing n=10
-- apply calls it with x=32 → 10+32 = 42
