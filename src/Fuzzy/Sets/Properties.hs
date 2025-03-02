module Fuzzy.Sets.Properties (
    -- * Standard predicates 
    isEmpty,
    isSingleton,
    isCrisp,
    isUniversal,
    strictSubsethood,
    strictEquality,
    -- * Graded predicates
    gradedSubsethood,
    gradedEquality,
) where

import FuzzySet
import Fuzzy.Sets.LSet
import Lattices.ResiduatedLattice 
import FuzzySet

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
isUniversal :: (FuzzySet set a l) => set -> Bool
isUniversal set = all (== top) [f x | x <- u]
    where 
        u = universe set
        f = member set


-- | Highest possible membership of a functions. 
-- It is always bound by the 'top' of the defining lattice.
height :: (FuzzySet set a l) => set -> l
height _ = top


-- | Support is a list of all items wit¨h non 'bot' 'member'
support :: (FuzzySet set a l) => set -> [a]
support set = filter ((/=bot) . f) u
    where f = member set
          u = universe set 


-- | Core of a fuzzy set is alpha cut where alpha = 'top'. 
-- In other words it's a list of items with 'member' equal to 'top'
core :: (FuzzySet set a l) => set -> [a]
core = alphaCut top

-- | Is 'FuzzySet' A subset of 'FuzzySet' B ?
strictSubsethood :: (FuzzySet set a l) => set -> set -> Bool
strictSubsethood set1 set2 = top == gradedSubsethood set1 set2


-- | Is 'FuzzySet' A equal to 'FuzzySet' B ?
strictEquality :: (FuzzySet set a l) => set -> set -> Bool
strictEquality set1 set2 = top == gradedEquality set1 set2 


-- | Degree to which set A is subset of B 
-- S(A,B) is commonly used syntax for this relation.
-- If S(A,B) = 1 we can conclude that A ⊆ B
gradedSubsethood :: (FuzzySet set a l) => set -> set -> l
gradedSubsethood set1 set2 = foldr (/\) top $ zipWith (-->) (map f u) (map g u)
        where 
            f = member set1
            g = member set2
            u = universe set1


-- | Degree to which set A is equal to the set B
-- e "A ≈ B is commonly used syntax for this relation
-- If A ≈ B = 1 than those fuzzy sets are equal
gradedEquality :: (FuzzySet set a l) => set -> set -> l
gradedEquality set1 set2 = foldr (/\) top $ zipWith (<-->) (map f u) (map g u)
        where 
            f = member set1
            g = member set2
            u = universe set1


{- $Standard predicates
Predicate functions that take a fuzzy set and return true if fuzzy set has some property (crisp, empty...)
-}

{- $Graded Predicates determining if two Fuzzy sets are equal can be... well, fuzzy. 
in this section we introduce predicates that return graded values from 'ResiduatedLattice'
-}