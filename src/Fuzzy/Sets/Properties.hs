module Fuzzy.Sets.Properties (
    isEmpty,
    isCrisp,
    isUniversal,
    gradedSubsethood,
    gradedEquality,
) where

import Fuzzy.Sets.LSet
import Lattices.ResiduatedLattice 

-- | Check if LSet is empty
isEmpty :: (ResiduatedLattice l) => LSet a l -> l
isEmpty (LSet f u)
    | x == top = top
    | otherwise = bot 
    where  x = foldr (/\) top [f x | x <- u]
    -- TODO: this function is not working as intended

-- | Check if fuzzy set is crisp set (Truth value structure contains only two values 0, 1)
isCrisp :: (ResiduatedLattice l) => LSet a l -> l
isCrisp (LSet f u)
    | crisp = top
    | otherwise = bot 
    where crisp = all (\x -> x == top || x == bot) [f x | x <- u]

-- | Fuzzy set is universal if it returns top for every value in its universe. 
-- This means that the fuzzy set equals its universe  
isUniversal :: (ResiduatedLattice l) => LSet a l -> l
isUniversal (LSet f u)
    | universal = top
    | otherwise = bot 
    where universal = all (== top) [f x | x <- u]

-- | Degree to which set A is subset of B 
-- S(A,B) is commonly used syntax for this relation.
-- If S(A,B) = 1 we can conclude that A ⊆ B
gradedSubsethood :: ResiduatedLattice l => LSet a l -> LSet a l -> l
gradedSubsethood = gradedOperation (-->)

-- | Degree to which set A is equal to the set B
-- e "A ≈ B is commonly used syntax for this relation
-- If A ≈ B = 1 than those fuzzy sets are equal
gradedEquality :: ResiduatedLattice l => LSet a l -> LSet a l -> l
gradedEquality = gradedOperation (<-->)

gradedOperation :: ResiduatedLattice l => (l -> l -> l) -> LSet a l -> LSet a l -> l
gradedOperation op (LSet f u) (LSet g _) =
    foldr (/\) bot $ zipWith op (map f u) (map g u)