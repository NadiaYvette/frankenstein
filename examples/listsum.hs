module ListSum where

data IntList = Nil | Cons Int IntList

sumList :: IntList -> Int
sumList Nil         = 0
sumList (Cons x xs) = x + sumList xs

main :: Int
main = sumList (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))))
