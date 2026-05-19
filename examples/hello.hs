module Hello where

-- Phase A hello-world for the GHC bridge.
-- main returns a String ([Char] cons-list); the MLIR main-wrapper
-- detects the cons-list return and emits kk_println_haskell_chars
-- which walks the list and prints each Char.
--
-- `putStrLn` and the IO monad are still unshimmed (GHC.Internal.IO.Handle
-- machinery — see ROADMAP → BRIDGE_haskell_strings), so the rest of
-- Haskell's standard string output remains unwired.
main :: String
main = "Hello, World!"
