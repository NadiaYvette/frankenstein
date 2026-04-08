module Tree where

data Tree = Leaf | Node Tree Int Tree

treeSum :: Tree -> Int
treeSum Leaf         = 0
treeSum (Node l n r) = treeSum l + n + treeSum r

main :: Int
main = treeSum (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))
