module Nested where

-- Single-constructor wrapper (newtype-like) nested inside a sum type.
-- The outer match discriminates Leaf vs Node; the inner Box wrapper
-- is deterministic once we know we're in the Node arm.
data Box = Box Int
data BoxTree = Leaf | Node BoxTree Box BoxTree

sumBoxTree :: BoxTree -> Int
sumBoxTree Leaf                 = 0
sumBoxTree (Node l (Box n) r)   = sumBoxTree l + n + sumBoxTree r

main :: Int
main = sumBoxTree (Node (Node Leaf (Box 10) Leaf) (Box 20) (Node Leaf (Box 30) Leaf))
