module TreeTransform where

data Tree a = Leaf a | Node (Tree a) (Tree a)

treeSize :: Tree a -> Int
treeSize (Leaf _) = 1
treeSize (Node l r) = 1 + treeSize l + treeSize r
