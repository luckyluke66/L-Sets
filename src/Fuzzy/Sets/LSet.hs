{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Fuzzy.Sets.LSet(
    LSet(LSet),
    FuzzySet(member, universe),
    fromPairs,
    fromFunction,
    crop,
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
    show :: (Show a, Show l, ResiduatedLattice l) => LSet a l -> String
    show (LSet f u) = "FuzzySet { " ++ show [(x, f x) | x <- u] ++ " }"

instance (ResiduatedLattice l, Eq a) => FuzzySet (LSet a l) a l where
    member :: ResiduatedLattice l => LSet a l -> a -> l
    member (LSet f _) = f 
    universe :: ResiduatedLattice l => LSet a l -> [a]
    universe (LSet _ u) = u
    mkFuzzySet :: (a -> l) -> [a] -> LSet a l
    mkFuzzySet = LSet
 

-- Construct fuzzy set from list of pairs
fromPairs :: (ResiduatedLattice l, Eq a) => [(a, l)] -> LSet a l
fromPairs xs = LSet f u
    where
        f x = fromMaybe bot (lookup x xs)
        u = map fst xs


-- Construct fuzzy set from a membership function and universe set
fromFunction :: (ResiduatedLattice l) => (a -> l) -> [a] -> LSet a l
fromFunction = LSet


-- | Round real number to 6 digits
crop :: RealFloat a => a -> a
crop x = fromInteger (round (x * (10^6))) / (10.0^^6)