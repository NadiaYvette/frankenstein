module HigherOrder where

-- Higher-order functions: closures, partial application, map/fold patterns.

data IntList = Nil | Cons Int IntList

myMap :: (Int -> Int) -> IntList -> IntList
myMap _ Nil         = Nil
myMap f (Cons x xs) = Cons (f x) (myMap f xs)

myFoldl :: (Int -> Int -> Int) -> Int -> IntList -> Int
myFoldl _ acc Nil         = acc
myFoldl f acc (Cons x xs) = myFoldl f (f acc x) xs

double :: Int -> Int
double x = x + x

addN :: Int -> Int -> Int
addN n x = n + x

main :: Int
main = myFoldl addN 0 (myMap double (Cons 1 (Cons 2 (Cons 3 Nil))))
-- double: [1,2,3] -> [2,4,6]
-- foldl addN 0 [2,4,6] = ((0+2)+4)+6 = 12
