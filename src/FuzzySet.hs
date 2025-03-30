{-# LANGUAGE FunctionalDependencies #-}

module FuzzySet(
    FuzzySet(..),
    alphaCut,
    union,
    unions,
    intersection,
    intersections,
    complement,
    setTnorm,
    setResiduum,
) where

import Lattices.ResiduatedLattice
import Lattices.UnitIntervalStructures.Lukasiewicz

-- | Type class defines the basic behavior for a fuzzy set
class (ResiduatedLattice l) => FuzzySet set a l | set -> a l where
    mkFuzzySet :: (a -> l) -> [a] -> set
    -- | membership function 
    member :: set -> a -> l
    universe :: set -> [a]
    truthDegrees :: set -> [l]
    truthDegrees set = [member set x | x <- universe set]
    universeCardinality :: set -> Int
    universeCardinality s = length $ universe s


-- | construct a empty fuzzy set 
mkEmptySet :: (FuzzySet set a l) => set
mkEmptySet = mkFuzzySet (const bot) []

-- | construct a singleton fuzzy set
mkSingletonSet :: (FuzzySet set a l, Eq a) => [a] -> (a, l) -> set
mkSingletonSet u (x, l) = mkFuzzySet f u
    where
        f y
            | y == x = l
            | otherwise = bot

-- | construct universal fuzzy set
mkUniversalSet :: (FuzzySet set a l, Eq a) => [a] -> set
mkUniversalSet = mkFuzzySet (const top)


-- | list of all values from 'universe' that have 'member' u >= alpha
alphaCut :: (FuzzySet set a l) => l -> set -> [a]
alphaCut alpha set = [x | x <- u, f x >= alpha]
    where f = member set
          u = universe set


{- | Fuzzy set union A ∪ B = C

==== __Examples__

>>> let set1 = fromPairs [(1, 0.2), (2, 0.7), (3, 0.1)] :: LSet Int UILukasiewicz 
    let set2 = fromPairs [(1, 0.3), (2, 0.4)] :: LSet Int UILukasiewicz 
    toPairs $ union set1 set2
[(1,0.3),(2,0.7), (3, 0.1)]

>>> let set1 = 
2
-}
union :: (FuzzySet set a l) => set -> set -> set
union set1 set2 = mkFuzzySet (\x -> f x \/ g x) u
    where f = member set1
          g = member set2
          u = universe set1


-- | 'union' over a list of sets
unions :: (FuzzySet set a l, Eq a) => [set] -> set
unions sets@(set:_) = foldr union (mkUniversalSet (universe set)) sets


-- | Fuzzy set intersection A ∩ B
intersection :: (FuzzySet set a l) => set -> set -> set
intersection set1 set2 = mkFuzzySet (\x ->  f x /\ g x) u
    where f = member set1
          g = member set2
          u = universe set1

-- | 'intersection' over a list of sets
intersections :: (FuzzySet set a l, Eq a) => [set] -> set
intersections = foldr intersection mkEmptySet

-- | Fuzzy set complement A′ 
complement :: (FuzzySet set a l) => set -> set
complement set = mkFuzzySet (negation . f)  u
    where f = member set
          u = universe set


-- | 'tnorm' over fuzzy sets 
setTnorm :: (FuzzySet set a l) => set -> set -> set
setTnorm set1 set2 = mkFuzzySet (\x -> f x `tnorm` g x) u
    where f = member set1
          g = member set2
          u = universe set1


-- | Residuum ('-->') over fuzzy sets
setResiduum :: (FuzzySet set a l) => set -> set -> set
setResiduum set1 set2 = mkFuzzySet (\x -> f x --> g x) u
    where f = member set1
          g = member set2
          u = universe set1


-- | Modify membership function of a fuzzy set by another function over the same same type
mapMembership :: (FuzzySet set a l) => set -> (a -> a) -> set
mapMembership set g = mkFuzzySet (f . g) u
    where 
        u = universe set
        f = member set


-- | Filter some values of a fuzzy set based on a predicate
filterMembership :: (FuzzySet set a l) => set -> (a -> Bool) -> set
filterMembership set pred =  mkFuzzySet h u
    where 
        h x = if pred x then f x else bot
        f = member set
        u = universe set


-- | Modify a universe of a fuzzy set.
mapU :: (FuzzySet set a l) => set -> (a -> a) -> set
mapU set g = mkFuzzySet f u
    where 
        f = member set
        u = map g (universe set)


-- | Filter universe of a fuzzy set.
filterU :: (FuzzySet set a l) => set -> (a -> Bool) -> set
filterU set pred = mkFuzzySet f u
    where 
        f = member set
        u = filter pred (universe set)


