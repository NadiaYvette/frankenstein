module MultiAdt where

-- Multiple ADTs interacting: Maybe with pairs.

data MyMaybe = MyNothing | MyJust Int
data Pair = MkPair Int Int

fromMaybe :: Int -> MyMaybe -> Int
fromMaybe d MyNothing  = d
fromMaybe _ (MyJust x) = x

fstPair :: Pair -> Int
fstPair (MkPair a _) = a

sndPair :: Pair -> Int
sndPair (MkPair _ b) = b

main :: Int
main = fromMaybe 0 (MyJust 10)
     + fromMaybe 7 MyNothing
     + fstPair (MkPair 100 200)
     + sndPair (MkPair 100 200)
-- 10 + 7 + 100 + 200 = 317
