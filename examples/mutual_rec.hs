module MutualRec where

-- Mutual recursion: isEven/isOdd with accumulator.

isEven :: Int -> Int
isEven 0 = 1
isEven n = isOdd (n - 1)

isOdd :: Int -> Int
isOdd 0 = 0
isOdd n = isEven (n - 1)

-- Sum isEven(i) for i in [0..9]:
-- even: 0,2,4,6,8 → 5 ones
-- odd:  1,3,5,7,9 → 0 ones
-- total = 5
countEven :: Int -> Int
countEven 0 = isEven 0
countEven n = isEven n + countEven (n - 1)

main :: Int
main = countEven 9
