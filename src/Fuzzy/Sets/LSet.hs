{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Fuzzy.Sets.LSet(
    LSet(LSet),
    FuzzySet(member, universe),
    fromPairs,
    fromFunction,
    toPairs,
    mkEmptySet,
    mkSingletonSet,
    mkUniversalSet
) where

import Lattices.UnitInterval
import Lattices.ResiduatedLattice
import FuzzySet
import Data.Maybe(fromMaybe)

-- | Fuzzy set A is a mapping from universe set U to the set of Truth values L
-- A: U -> L this function is called 'membership' function
data (ResiduatedLattice l) => LSet a l = LSet
    {
    -- | membership function 
     membership :: a -> l
    -- | universe set
    , universe :: ![a]
    }


instance (Eq a, Show a, Show l, ResiduatedLattice l) => Show (LSet a l) where
    show :: LSet a l -> String
    show set = "FuzzySet { " ++ show (toPairs set) ++ " }"


instance (ResiduatedLattice l, Eq a) => FuzzySet (LSet a l) a l where
    member :: LSet a l -> a -> l
    member (LSet f _) = f 
    universe :: LSet a l -> [a]
    universe (LSet _ u) = u
    mkFuzzySet :: (a -> l) -> [a] -> LSet a l
    mkFuzzySet = LSet


-- | Construct fuzzy set from list of pairs
fromPairs :: (ResiduatedLattice l, Eq a) => [(a, l)] -> LSet a l
fromPairs xs = LSet f u
    where
        f x = fromMaybe bot (lookup x xs)
        u = map fst xs


{- | Construct a fuzzy set from a membership function and a universe set

==== __Examples__

>>> let f x = if x == 1 then 0.8 else 0.3
>>> let set = fromFunction f [1, 2, 3] :: LSet Int UILukasiewicz
>>> toPairs set
[(1,0.8),(2,0.3),(3,0.3)]
-}
fromFunction :: (ResiduatedLattice l) => (a -> l) -> [a] -> LSet a l
fromFunction = LSet


-- | Convert fuzzy set to list of pairs 
toPairs :: (ResiduatedLattice l, Eq a) => LSet a l -> [(a, l)]
toPairs (LSet f universe) = [(u, f u) | u <- universe]


{- | Construct an empty fuzzy set

==== __Examples__

>>> let emptySet = mkEmptySet :: LSet Int UILukasiewicz
>>> toPairs emptySet
[]
-}
mkEmptySet :: (ResiduatedLattice l) => LSet a l
mkEmptySet = LSet (const bot) []

{- | Construct a singleton fuzzy set

==== __Examples__

>>> let singletonSet = mkSingletonSet [1, 2, 3] (2, 0.8) :: LSet Int UILukasiewicz
>>> toPairs singletonSet
[(1,0.0),(2,0.8),(3,0.0)]
-}
mkSingletonSet :: (ResiduatedLattice l, Eq a) => [a] -> (a, l) -> LSet a l
mkSingletonSet u (x, l) = LSet f u
    where
        f y
            | y == x = l
            | otherwise = bot


{- | Construct a universal fuzzy set

==== __Examples__

>>> let universalSet = mkUniversalSet [1, 2, 3] :: LSet Int UILukasiewicz
>>> toPairs universalSet
[(1,1.0),(2,1.0),(3,1.0)]
-}
mkUniversalSet :: (ResiduatedLattice l, Eq a) => [a] -> LSet a l
mkUniversalSet = LSet (const top)
