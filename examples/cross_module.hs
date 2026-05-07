module CrossModule where

-- Test: Cross-module Haskell compilation.
-- Imports CrossModuleLib which defines double, triple, and sumList.
-- The GHC bridge should compile both modules in one session via
-- compileToCoreMulti, then the linker merges them.

import CrossModuleLib (double, triple, sumList)

main :: Int
main = double 10 + triple 5 + sumList [1, 2, 3, 4]
-- Expected: 20 + 15 + 10 = 45
