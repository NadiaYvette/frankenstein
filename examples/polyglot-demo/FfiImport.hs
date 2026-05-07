-- | Haskell module that imports foreign functions via `foreign import ccall`.
-- These resolve at link time to functions from other languages (Python, Go).
module FfiImport where

foreign import ccall "square" c_square :: Int -> Int
foreign import ccall "gcd" c_gcd :: Int -> Int -> Int

-- | square(gcd(a, b))
-- Function form (not CAF) to avoid lazy thunk wrapping in cross-language calls.
squaredGcd :: Int -> Int -> Int
squaredGcd a b = c_square (c_gcd a b)
