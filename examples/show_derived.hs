module ShowDerived where

-- Demonstrates that `deriving Show` works for three common ADT shapes
-- when each is in its own module-like Show scope.  GHC's deriving
-- generates a $cshowsPrec method body that references several stdlib
-- helpers: ++, itos, showSpace, $fShowCallStack{2,3} (parens), and
-- the prefix CAFs holding constructor names.
--
-- Currently working:
--   - Enum-only ADTs (nullary constructors)
--   - Single-constructor ADTs with args
--   - Multi-constructor ADTs with all branches taking args
--
-- Known limitation: mixing enum and with-args ADTs in the same module
-- triggers a lambda-lifting helper-name collision — see ROADMAP →
-- BRIDGE_haskell_strings.

data Tree = Leaf | Node Tree Int Tree deriving Show

main :: IO ()
main = print (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))
