module LazyPair where

data Pair a b = Pair a b

first :: Pair a b -> a
first (Pair x _) = x

lazyThunk :: Int -> Int
lazyThunk n =
  let x = n + 1
      y = n * 2
  in x + y
