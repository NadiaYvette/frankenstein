module BoolGuardsWhere where

-- Pattern class: function with Bool guards over where-bound helpers,
-- where the helpers themselves use pattern matching (multi-clause or
-- case-of-data-constructor). This is the shape used by the failed
-- round-13 classifyBranches refactor.

data MaybeI = NoI | YesI Int

classify :: [MaybeI] -> Maybe MaybeI -> Int
classify ms mDefault
  | exactlyOne && noDefault && firstIsYes = unwrapFirst
  | otherwise                              = 0
  where
    exactlyOne = length ms == 1
    noDefault = case mDefault of
      Nothing -> True
      Just _  -> False
    firstIsYes = case ms of
      (YesI _ : _) -> True
      _            -> False
    unwrapFirst = case ms of
      (YesI x : _) -> x
      _            -> 0

main :: Int
main = classify [YesI 42] Nothing
-- Expect: 42
