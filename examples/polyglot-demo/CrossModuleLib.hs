module CrossModuleLib where

-- Haskell library module: pure numeric helpers.
-- Compiled through the GHC bridge's multi-module compilation,
-- then called from Koka via extern declarations.

double :: Int -> Int
double x = x + x

triple :: Int -> Int
triple x = x + x + x

sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList xs
