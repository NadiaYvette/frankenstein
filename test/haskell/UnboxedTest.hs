module UnboxedTest where

-- Simple arithmetic that GHC will optimize to unboxed Int# ops
sumTo :: Int -> Int
sumTo 0 = 0
sumTo n = n + sumTo (n - 1)

-- This should produce a strict worker with Int# args at -O1
main :: IO ()
main = print (sumTo 100)
