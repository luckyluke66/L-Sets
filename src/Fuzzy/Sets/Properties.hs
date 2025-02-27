module Fuzzy.Sets.Properties (
    isEmpty,
    isSingleton,
    isCrisp,
    isUniversal,
    gradedSubsethood,
    gradedEquality,
) where

import FuzzySet
import Fuzzy.Sets.LSet
import Lattices.ResiduatedLattice 

--  | True if 'fuzzySet' is empty, meaning for each u in 'universe' 
-- 'member' u == 'bot'
isEmpty :: (FuzzySet set a l) => set -> Bool
isEmpty set = all (== bot) (truthDegrees set)


-- | 'fuzzySet' is a singleton if and only if there is exactly 
-- one u in 'universe' for which 'member' u is greater than 'bot'
isSingleton :: (FuzzySet set a l) => set -> Bool
isSingleton set = length (filter (> bot) (truthDegrees set)) == 1


-- | Check if fuzzy set is crisp set (Truth value structure contains only two values 0, 1)
isCrisp :: (FuzzySet set a l) => set -> Bool
isCrisp set = all (\x -> x == top || x == bot) [f x | x <- u]
    where
        f = member set
        u = universe set

-- | Fuzzy set is universal if it returns top for every value in its universe. 
-- This means that the fuzzy set equals its universe  
isUniversal :: (FuzzySet set a l) => set -> l
isUniversal set
    | universal = top
    | otherwise = bot 
    where 
        universal = all (== top) [f x | x <- u]
        u = universe set
        f = member set

-- | Degree to which set A is subset of B 
-- S(A,B) is commonly used syntax for this relation.
-- If S(A,B) = 1 we can conclude that A ⊆ B
gradedSubsethood :: (FuzzySet set a l) => set -> set -> l
gradedSubsethood = gradedOperation (-->)

-- | Degree to which set A is equal to the set B
-- e "A ≈ B is commonly used syntax for this relation
-- If A ≈ B = 1 than those fuzzy sets are equal
gradedEquality :: (FuzzySet set a l) => set -> set -> l
gradedEquality = gradedOperation (<-->)

gradedOperation :: (FuzzySet set a l) => (l -> l -> l) -> set -> set -> l
gradedOperation op set1 set2 =
    foldr (/\) bot $ zipWith op (map f u) (map g u)
        where 
            f = member set1
            g = member set2
            u = universe set1
