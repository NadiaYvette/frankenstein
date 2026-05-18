module SinglePatLit where

-- Pattern class: case with exactly ONE branch using PatLit (integer literal).
-- Like single_patcon.hs but with a literal pattern.

classify :: Int -> Int
classify n = case n of
  0 -> 42

main :: Int
main = classify 0
-- Expect: 42 (non-exhaustive but the input matches)
