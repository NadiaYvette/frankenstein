module PreludeHof where

-- Test: Higher-order functions on standard [Int] lists.
-- Uses locally-defined map/filter/foldr as a test of custom HOFs.
-- (Real Prelude map/filter/foldr also work — see prelude_comprehensive.hs)
-- The key test: HOFs + stdlib list constructors + Perceus RC.

myMap :: (Int -> Int) -> [Int] -> [Int]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (Int -> Bool) -> [Int] -> [Int]
myFilter _ [] = []
myFilter p (x:xs)
  | p x       = x : myFilter p xs
  | otherwise  = myFilter p xs

myFoldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int
myFoldr _ z []     = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

double :: Int -> Int
double x = x + x

isPositive :: Int -> Bool
isPositive n = n > 0

add :: Int -> Int -> Int
add a b = a + b

main :: Int
main = myFoldr add 0 (myMap double (myFilter isPositive [negate 1, 2, negate 3, 4, 5]))
-- filter isPositive = [2, 4, 5]
-- map double = [4, 8, 10]
-- foldr (+) 0 = 22
-- Expected: 22
