{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Fuzzy.Sets.LSet(
    LSet(LSet),
    FuzzySet(member, universe),
    crop,
    union,
    intersection,
    complement,
    alphaCut,
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


-- | Fuzzy set union A ∪ B = C
union :: (ResiduatedLattice l) => LSet a l -> LSet a l -> LSet a l
union (LSet f u) (LSet g _) = LSet (\x -> f x \/ g x) u

-- | Fuzzy set intersection A ∩ B
intersection :: (ResiduatedLattice l) => LSet a l -> LSet a l -> LSet a l
intersection (LSet f u) (LSet g _) = LSet (\x ->  f x /\ g x) u

-- | Fuzzy set complement A′ 
complement :: (ResiduatedLattice l, Num l) => LSet a l -> LSet a l
complement (LSet f u) = LSet (negation . f)  u

-- | 'tnorm' over fuzzy sets 
setTnorm :: (ResiduatedLattice l) => LSet a l -> LSet a l -> LSet a l
setTnorm (LSet f u) (LSet g _) = LSet (\x -> f x `tnorm` g x) u

-- | Residuum ('-->') over fuzzy sets
setResiduum :: (ResiduatedLattice l) => LSet a l -> LSet a l -> LSet a l
setResiduum (LSet f u) (LSet g _) = LSet (\x -> f x --> g x) u


-- | Alpha cut on Fuzzy set A is a list of items from universe U that 
-- belong to A in degree at least alpha
alphaCut :: (ResiduatedLattice l) => l -> LSet a l -> [a]
alphaCut alpha (LSet f u) = [x | x <- u, f x >= alpha]