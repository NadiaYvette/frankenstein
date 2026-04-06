module JoinPointTest where

-- Guards and nested patterns create join points in GHC Core
classify :: Int -> Int
classify x
  | x < 0     = -1
  | x == 0    = 0
  | otherwise  = 1

main :: IO ()
main = print (classify (-5) + classify 0 + classify 42)
