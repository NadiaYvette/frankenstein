module StdlibMaybe where

-- Test: Use Haskell's standard Maybe type from Prelude.
-- This requires the GHC bridge to extract the Maybe TyCon
-- from Core and generate correct constructor tags for Just/Nothing.

fromMaybe :: Int -> Maybe Int -> Int
fromMaybe def Nothing  = def
fromMaybe _   (Just x) = x

main :: Int
main = fromMaybe 0 (Just 42) + fromMaybe 99 Nothing
-- Expected: 42 + 99 = 141
