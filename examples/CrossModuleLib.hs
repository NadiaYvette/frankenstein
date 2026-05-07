module CrossModuleLib where

-- A library module imported by cross_module.hs.
-- Tests multi-module GHC bridge compilation: the bridge should
-- chase this import and compile both modules in one session.

double :: Int -> Int
double x = x + x

triple :: Int -> Int
triple x = x + x + x

sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList xs
