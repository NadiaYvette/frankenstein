module MultiPatConExhaustive where

-- Pattern class: case with multiple branches, each PatCon, no default.
-- normalizePatterns appends a synthetic PatWild default.
--
-- Expected: classifyBranches → ConCase _ (Just default) (already normalized).
-- Both host and self-host SHOULD agree (normalize forces same shape).

data Color = Red | Green | Blue

intOfColor :: Color -> Int
intOfColor c = case c of
  Red   -> 1
  Green -> 2
  Blue  -> 3

main :: Int
main = intOfColor Green
-- Expect: 2
