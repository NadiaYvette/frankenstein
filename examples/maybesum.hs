module MaybeSum where

data MyMaybe = MyNothing | MyJust Int

orZero :: MyMaybe -> Int
orZero MyNothing  = 0
orZero (MyJust x) = x

main :: Int
main = orZero (MyJust 7) + orZero MyNothing + orZero (MyJust 35)
