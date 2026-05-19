module ShowMixed where

-- Demonstrates that multiple `deriving Show` ADTs with different shapes
-- in one module work correctly.  Previously this triggered a
-- lambda-lifter helper-name collision: GHC's simplifier shares the
-- same Unique for `let go1 = ...` bindings introduced in different
-- $cshowsPrec bodies, even when their free-variable contexts differ
-- (enum branches don't capture `z`, sum-with-args branches do).
-- The MLIR emitter's lambda-lifter deduplicated by name, picked one
-- capture set, and the other context's call sites emitted the wrong
-- argument count.
--
-- Fix (in GhcBridge.CoreTranslate.disambiguateLetBindings): every
-- let-binding gets a fresh Unique by adding a per-site counter
-- offset, so no two binding sites ever share a name.

data Color = Red | Green | Blue deriving Show
data Point = Point Int Int deriving Show
data Shape = Circle Int | Rectangle Int Int | Square Int deriving Show

main :: IO ()
main = do
  print Red
  print Green
  print Blue
  print (Point 3 4)
  print (Circle 10)
  print (Rectangle 7 13)
  print (Square 5)
