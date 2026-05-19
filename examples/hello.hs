module Hello where

-- Phase A hello-world for the GHC bridge — degraded form.
--
-- The GHC bridge does not yet shim GHC.Internal.IO.Handle.Text, so the
-- natural `main = putStrLn "Hello, World!"` fails to link.  See ROADMAP's
-- "Phase 9 Outstanding Issues" → BRIDGE_haskell_strings.
--
-- This degraded form exercises the *string literal allocation* path
-- (unpackCString# → cons-list of Char) and prints the cons-list length,
-- which routes through the proven main-returns-Int wrapper.
--
-- Expected output: 13   (length of "Hello, World!")

myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

main :: Int
main = myLength "Hello, World!"
