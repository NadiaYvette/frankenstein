{-# LANGUAGE MagicHash #-}
module ArithPrim where

import GHC.Exts

addInt :: Int# -> Int# -> Int#
addInt x y = x +# y

mulInt :: Int# -> Int# -> Int#
mulInt x y = x *# y
