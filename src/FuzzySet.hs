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
import qualified Data.List as SetOp(union, intersect)

-- | Type class defines the basic behavior for a fuzzy set
class (ResiduatedLattice l, Eq a) => FuzzySet set a l | set -> a l where
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


{- | list of all values from 'universe' that have ('member' u) >= alpha

==== __Examples__

>>> let set = fromPairs [(1, 0.1), (2, 0.2), (3. 0.4)]
>>> alphaCut 0.15 set 
[2, 3]

>>> alphaCut 0.3 set
[3]

>>> alphaCut 0.5 set
[]
>>> alphaCut 1 (mkUniversalSet [1..10])
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 
-}
alphaCut :: (FuzzySet set a l) => l -> set -> [a]
alphaCut alpha set = [x | x <- u, f x >= alpha]
    where f = member set
          u = universe set


{- | Fuzzy set union A ∪ B. Universe of the new set is union of universes from A and B.

==== __Examples__

>>> let set1 = fromPairs [(1, 0.2), (2, 0.7), (3, 0.1)] :: LSet Int UILukasiewicz 
>>> let set2 = fromPairs [(1, 0.3), (2, 0.4)] :: LSet Int UILukasiewicz 
>>> let set3 = fromPairs [(1, 0.5), (2, 0.1), (4, 0.8)] :: LSet Int UILukasiewicz
>>> toPairs $ union set1 set2
[(1, 0.3),(2, 0.7), (3, 0.1)]

>>> toPairs $ union set1 set3
[(1, 0.5), (2, 0.7), (3, 0.1), (4, 0.8)]

>>> toPairs $ union set1 mkEmptySet
[(1, 0.2), (2, 0.7), (3, 0.1)]
-}
union :: (FuzzySet set a l) => set -> set -> set
union set1 set2 = mkFuzzySet (\x -> f x \/ g x) u
    where f = member set1
          g = member set2
          u = SetOp.union (universe set1) (universe set2)


-- | 'union' over a list of sets
unions :: (FuzzySet set a l, Eq a) => [set] -> set
unions sets@(set:_) = foldr union (mkUniversalSet (universe set)) sets


{- | Fuzzy set intersection A ∩ B. Universe of the new set is intersection of universes from A and B.

==== __Examples__

>>> let set1 = fromPairs [(1, 0.2), (2, 0.7), (3, 0.1)]
>>> let set2 = fromPairs [(1, 0.3), (2, 0.4)]
>>> let set3 = fromPairs [(1, 0.5), (2, 0.1), (4, 0.8)] :: LSet Int UILukasiewicz
>>> toPairs $ intersection set1 set2
[(1, 0.2), (2, 0.4), (3, 0.0)]

>>> toPairs $ intersection set1 set3
[(1, 0.2), (2, 0.1), (3, 0.0), (4, 0.0)]

>>> toPairs $ intersection set1 mkEmptySet
[(1, 0.0), (2, 0.0), (3, 0.0)]
-}
intersection :: (FuzzySet set a l) => set -> set -> set
intersection set1 set2 = mkFuzzySet (\x ->  f x /\ g x) u
    where f = member set1
          g = member set2
          u = SetOp.intersect (universe set1) (universe set2)

-- | 'intersection' over a list of sets
intersections :: (FuzzySet set a l, Eq a) => [set] -> set
intersections = foldr intersection mkEmptySet

{- | Complement of a fuzzy set A'

==== __Examples__

>>> let set1 = fromPairs [(1, 0.2), (2, 0.7)] :: LSet Int UILukasiewicz
>>> toPairs $ complement set1
[(1, 0.8),(2, 0.3)]

>>> let set2 = fromPairs [(1, 1), (2, 1)]
>>> toPairs $ complement set2
[(1, 0), (2, 0)]
-}
complement :: (FuzzySet set a l) => set -> set
complement set = mkFuzzySet (negation . f)  (universe set)
    where f = member set


{- | Apply a t-norm operation over two fuzzy sets. Both sets should be defined on the same 'universe'.

==== __Examples__

>>> let set1 = fromPairs [(1, 0.2), (2, 0.7)] :: LSet Int UILukasiewicz
>>> let set2 = fromPairs [(1, 0.3), (2, 0.4)] :: LSet Int UILukasiewicz
>>> toPairs $ setTnorm set1 set2
[(1,0.2), (2,0.4)]
-}
setTnorm :: (FuzzySet set a l) => set -> set -> set
setTnorm set1 set2 = mkFuzzySet (\x -> f x `tnorm` g x) u
    where f = member set1
          g = member set2
          u = universe set1


{- | Apply a residuum operation over two fuzzy sets. Both sets should be defined on the same 'universe'.

==== __Examples__

>>> let set1 = fromPairs [(1, 0.2), (2, 0.7)] :: LSet Int UILukasiewicz
>>> let set2 = fromPairs [(1, 0.3), (2, 0.4)] :: LSet Int UILukasiewicz
>>> toPairs $ setResiduum set1 set2
[(1,1.0), (2,0.7)]
-} 
setResiduum :: (FuzzySet set a l) => set -> set -> set
setResiduum set1 set2 = mkFuzzySet (\x -> f x --> g x) u
    where f = member set1
          g = member set2
          u = universe set1


{- | Modify the membership function of a fuzzy set by applying another function to its elements

==== __Examples__

>>> let set = fromPairs [(1, 0.2), (2, 0.7)] :: LSet Int UILukasiewicz
>>> let modifiedSet = mapMembership set (\x -> x + 1)
>>> toPairs modifiedSet
[(1,0.0),(2,0.0),(3,0.2)]
-}
mapMembership :: (FuzzySet set a l) => set -> (a -> a) -> set
mapMembership set g = mkFuzzySet (f . g) u
    where 
        u = universe set
        f = member set


{- | Filter values of a fuzzy set based on a predicate

==== __Examples__

>>> let set = fromPairs [(1, 0.2), (2, 0.7), (3, 0.4)] :: LSet Int UILukasiewicz
>>> let filteredSet = filterMembership set (\x -> x > 1)
>>> toPairs filteredSet
[(1,0.0),(2,0.7),(3,0.4)]
-}
filterMembership :: (FuzzySet set a l) => set -> (a -> Bool) -> set
filterMembership set pred =  mkFuzzySet h u
    where 
        h x = if pred x then f x else bot
        f = member set
        u = universe set


{- | Modify the universe of a fuzzy set by applying a function to its elements

==== __Examples__

>>> let set = fromPairs [(1, 0.2), (2, 0.7)] :: LSet Int UILukasiewicz
>>> let modifiedSet = mapU set (\x -> x * 2)
>>> toPairs modifiedSet
[(2,0.2),(4,0.7)]
-}
mapU :: (FuzzySet set a l) => set -> (a -> a) -> set
mapU set g = mkFuzzySet f u
    where 
        f = member set
        u = map g (universe set)


{- | Filter the universe of a fuzzy set based on a predicate

==== __Examples__

>>> let set = fromPairs [(1, 0.2), (2, 0.7), (3, 0.4)] :: LSet Int UILukasiewicz
>>> let filteredSet = filterU set (\x -> x > 1)
>>> toPairs filteredSet
[(2,0.7),(3,0.4)]
-}
filterU :: (FuzzySet set a l) => set -> (a -> Bool) -> set
filterU set pred = mkFuzzySet f u
    where 
        f = member set
        u = filter pred (universe set)
