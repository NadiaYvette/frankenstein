module ExhaustTail where

data Pair = MkPair Int Int

getPairSum :: Pair -> Int
getPairSum (MkPair a b) = a + b

data Triple = MkTriple Int Int Int

getTripleSum :: Triple -> Int
getTripleSum (MkTriple a b c) = a + b + c

main :: Int
main = getPairSum (MkPair 10 20) + getTripleSum (MkTriple 1 2 3)
