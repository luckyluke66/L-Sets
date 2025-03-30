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


instance (Show a, Show l, ResiduatedLattice l) => Show (LSet a l) where
    show :: LSet a l -> String
    show (LSet f u) = "FuzzySet { " ++ show [(x, f x) | x <- u] ++ " }"


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


-- | Construct fuzzy set from a membership function and universe set
fromFunction :: (ResiduatedLattice l) => (a -> l) -> [a] -> LSet a l
fromFunction = LSet


-- | Convert fuzzy set to list of pairs 
toPairs :: (ResiduatedLattice l, Eq a) => LSet a l -> [(a, l)]
toPairs (LSet f universe) = [(u, f u) | u <- universe]


-- | construct a empty fuzzy set 
mkEmptySet :: (ResiduatedLattice l) => LSet a l
mkEmptySet = LSet (const bot) []

-- | construct a singleton fuzzy set
mkSingletonSet :: (ResiduatedLattice l, Eq a) => [a] -> (a, l) -> LSet a l
mkSingletonSet u (x, l) = LSet f u
    where
        f y
            | y == x = l
            | otherwise = bot

-- | construct universal fuzzy set
mkUniversalSet :: (ResiduatedLattice l, Eq a) => [a] -> LSet a l
mkUniversalSet = LSet (const top)
