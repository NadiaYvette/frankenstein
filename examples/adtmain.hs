module AdtMain where

data Tree = Leaf | Node Tree Int Tree

main :: Tree
main = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
