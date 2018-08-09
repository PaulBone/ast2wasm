
module Util where

listMaybeToList :: [Maybe a] -> [a]
listMaybeToList [] = []
listMaybeToList (Nothing:xs) = listMaybeToList xs
listMaybeToList (Just x:xs) = x:(listMaybeToList xs)

