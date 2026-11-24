module HigherOrder where

mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs

foldList :: (b -> a -> b) -> b -> [a] -> b
foldList _ acc [] = acc
foldList f acc (x:xs) = foldList f (f acc x) xs
