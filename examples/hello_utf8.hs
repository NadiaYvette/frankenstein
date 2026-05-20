module HelloUtf8 where

-- Non-ASCII string literals through the GHC bridge.  Each literal
-- is decoded as UTF-8 by `unpackLitStringToCons` into a [Char]
-- cons-list of Unicode codepoints.  The runtime's
-- `kk_print_haskell_chars` UTF-8-encodes each codepoint to stdout,
-- so multi-byte chars round-trip correctly.
--
-- The string literal that contains non-ASCII bytes triggers GHC's
-- `unpackFoldrCStringUtf8#` form (the foldr-style unpacker emitted
-- after the simplifier inlines string list folds).  The bridge
-- recognises that form alongside the simpler `unpackCStringUtf8#`.

main :: IO ()
main = do
  putStrLn "ascii hello"
  putStrLn "café"
  putStrLn "한국어"
  putStrLn "🎉 party"
