module FuzzySets.Properties (
    isEmpty,
    isCrisp,
    isUniversal,
    gradedSubsethood,
    gradedEquality,
) where

import FuzzySets.FuzzySet
import Lattices.ResiduatedLattice 

isEmpty :: (ResiduatedLattice l) => FuzzySet a l -> l
isEmpty (FuzzySet f u)
    | x == top = top
    | otherwise = bot 
    where  x= foldr (/\) top [f x | x <- u]

isCrisp :: (ResiduatedLattice l) => FuzzySet a l -> l
isCrisp (FuzzySet f u)
    | crisp = top
    | otherwise = bot 
    where crisp = all (\x -> x == top || x == bot) [f x | x <- u]

isUniversal :: (ResiduatedLattice l) => FuzzySet a l -> l
isUniversal (FuzzySet f u)
    | universal = top
    | otherwise = bot 
    where universal = all (== top) [f x | x <- u]

gradedSubsethood :: ResiduatedLattice l => FuzzySet a l -> FuzzySet a l -> l
gradedSubsethood = gradedOperation (-->)

gradedEquality :: ResiduatedLattice l => FuzzySet a l -> FuzzySet a l -> l
gradedEquality = gradedOperation (<-->)

gradedOperation :: ResiduatedLattice l => (l -> l -> l) -> FuzzySet a l -> FuzzySet a l -> l
gradedOperation op (FuzzySet f u) (FuzzySet g _) =
    foldr (/\) bot $ zipWith op (map f u) (map g u)