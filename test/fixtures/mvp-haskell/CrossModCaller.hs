module CrossModCaller where

import TreeTransform (Tree(..), treeSize)

compute :: Tree Int -> Int
compute t = treeSize t * 2
