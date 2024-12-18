{-# LANGUAGE InstanceSigs #-}

module FuzzySets.FuzzySet(
    FuzzySet(FuzzySet),
    membership,
    universe,
    crop,
    union,
    intersection,
    complement,
    alphaCut,
) where

import Data.Maybe
import Lattices.UnitInterval
import Lattices.ResiduatedLattice


data (ResiduatedLattice l) => FuzzySet a l = FuzzySet
    { membershipFunction :: a -> l
    , universeSet :: ![a]
    }

instance (Show a, Show l, ResiduatedLattice l) => Show (FuzzySet a l) where
    show :: (Show a, Show l, ResiduatedLattice l) => FuzzySet a l -> String
    show (FuzzySet f u) = "FuzzySet { " ++ show [(x, f x) | x <- u] ++ " }"


fromPairs :: (ResiduatedLattice l, Eq a) => [(a, l)] -> FuzzySet a l
fromPairs xs = FuzzySet f u
    where
        f x = fromMaybe bot (lookup x xs)
        u = map fst xs

fromFunction :: (ResiduatedLattice l) => (a -> l) -> [a] -> FuzzySet a l
fromFunction = FuzzySet

membership :: (ResiduatedLattice l) => FuzzySet a l -> a -> l
membership (FuzzySet f _) = f

universe :: (ResiduatedLattice l) => FuzzySet a l -> [a]
universe (FuzzySet _ u) = u

crop :: RealFloat a => a -> a
crop x = fromInteger (round (x * (10^6))) / (10.0^^6)

union :: (ResiduatedLattice l) => FuzzySet a l -> FuzzySet a l -> FuzzySet a l
union (FuzzySet f u) (FuzzySet g _) = FuzzySet (\x -> f x \/ g x) u

intersection :: (ResiduatedLattice l) => FuzzySet a l -> FuzzySet a l -> FuzzySet a l
intersection (FuzzySet f u) (FuzzySet g _) = FuzzySet (\x ->  f x /\ g x) u

complement :: (ResiduatedLattice l, Num l) => FuzzySet a l -> FuzzySet a l
complement (FuzzySet f u) = FuzzySet (negation . f)  u

setTnorm :: (ResiduatedLattice l) => FuzzySet a l -> FuzzySet a l -> FuzzySet a l
setTnorm (FuzzySet f u) (FuzzySet g _) = FuzzySet (\x -> f x `tnorm` g x) u

setResiduum :: (ResiduatedLattice l) => FuzzySet a l -> FuzzySet a l -> FuzzySet a l
setResiduum (FuzzySet f u) (FuzzySet g _) = FuzzySet (\x -> f x --> g x) u

alphaCut :: (ResiduatedLattice l) => l -> FuzzySet a l -> [a]
alphaCut alpha (FuzzySet f u) = [x | x <- u, f x >= alpha]