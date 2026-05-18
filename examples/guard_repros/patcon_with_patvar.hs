module PatConWithPatVar where

-- Pattern class: case with PatCon branches PLUS an explicit PatVar default.
-- This is the "ConCase ... (Just default)" path without normalize touching it.

data Maybe2 = Nothing2 | Just2 Int

unwrapOr :: Int -> Maybe2 -> Int
unwrapOr d m = case m of
  Just2 x -> x
  _other  -> d

main :: Int
main = unwrapOr 99 (Just2 42)
-- Expect: 42
