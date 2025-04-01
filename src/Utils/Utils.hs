module Utils.Utils(
    universeToList,
    crop,
    listToUniverse
) where

import Data.List(nub)

flattenPairs :: [(a, a)] -> [a]
flattenPairs = foldr (\(x ,y) -> (++) [x, y]) []

universeToList :: (Eq a) => [(a, a)] -> [a]
universeToList = nub . flattenPairs

listToUniverse :: (Eq a) => [a] -> [(a, a)] 
listToUniverse u = [(x, y) | x <- u, y <- u]

-- | Round real number to 6 digits
crop :: RealFloat a => a -> a
crop x = fromInteger (round (x * (10^6))) / (10.0^^6)
