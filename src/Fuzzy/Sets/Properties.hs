module Fuzzy.Sets.Properties (
    isEmpty,
    isCrisp,
    isUniversal,
    gradedSubsethood,
    gradedEquality,
) where

import Fuzzy.Sets.LSet
import Lattices.ResiduatedLattice 

isEmpty :: (ResiduatedLattice l) => LSet a l -> l
isEmpty (LSet f u)
    | x == top = top
    | otherwise = bot 
    where  x = foldr (/\) top [f x | x <- u]

isCrisp :: (ResiduatedLattice l) => LSet a l -> l
isCrisp (LSet f u)
    | crisp = top
    | otherwise = bot 
    where crisp = all (\x -> x == top || x == bot) [f x | x <- u]

isUniversal :: (ResiduatedLattice l) => LSet a l -> l
isUniversal (LSet f u)
    | universal = top
    | otherwise = bot 
    where universal = all (== top) [f x | x <- u]

gradedSubsethood :: ResiduatedLattice l => LSet a l -> LSet a l -> l
gradedSubsethood = gradedOperation (-->)

gradedEquality :: ResiduatedLattice l => LSet a l -> LSet a l -> l
gradedEquality = gradedOperation (<-->)

gradedOperation :: ResiduatedLattice l => (l -> l -> l) -> LSet a l -> LSet a l -> l
gradedOperation op (LSet f u) (LSet g _) =
    foldr (/\) bot $ zipWith op (map f u) (map g u)