module Hello where

-- Phase A hello-world for the GHC bridge.
-- main calls putStrLn; GHC's simplifier inlines this to
--   delay (hPutStr2 stdout cons_list True)
-- which the bridge intercepts (ghcIoOutputRuntime in
-- Frankenstein.GhcBridge.CoreTranslate) and rewrites to a direct
-- println_haskell_chars(cons_list) call.  The runtime helper walks
-- the [Char] cons-list and putchar's each codepoint, appending a
-- trailing newline.
main :: IO ()
main = putStrLn "Hello, World!"
