module Utils.Utils(
    universeToList
) where

import Data.List(nub)

flattenPairs :: [(a, a)] -> [a]
flattenPairs = foldr (\(x ,y) -> (++) [x, y]) []

universeToList :: (Eq a) => [(a, a)] -> [a]
universeToList = nub . flattenPairs