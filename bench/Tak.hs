module Tak where

tak :: Int -> Int -> Int -> Int
tak x y z
  | y < x     = tak (tak (x - 1) y z) (tak (y - 1) z x) (tak (z - 1) x y)
  | otherwise  = z

main :: IO ()
main = print (tak 24 16 8)
