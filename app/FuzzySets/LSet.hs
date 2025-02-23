{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FuzzySets.LSet(
    LSet(LSet),
    crop,
    union,
    intersection,
    complement,
    alphaCut,
    FuzzySet(member, universe)
) where

import Lattices.UnitInterval
import Lattices.ResiduatedLattice
import FuzzySet
import Data.Maybe(fromMaybe)

data (ResiduatedLattice l) => LSet a l = LSet
    { membership :: a -> l
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

fromPairs :: (ResiduatedLattice l, Eq a) => [(a, l)] -> LSet a l
fromPairs xs = LSet f u
    where
        f x = fromMaybe bot (lookup x xs)
        u = map fst xs

fromFunction :: (ResiduatedLattice l) => (a -> l) -> [a] -> LSet a l
fromFunction = LSet



crop :: RealFloat a => a -> a
crop x = fromInteger (round (x * (10^6))) / (10.0^^6)

union :: (ResiduatedLattice l) => LSet a l -> LSet a l -> LSet a l
union (LSet f u) (LSet g _) = LSet (\x -> f x \/ g x) u

intersection :: (ResiduatedLattice l) => LSet a l -> LSet a l -> LSet a l
intersection (LSet f u) (LSet g _) = LSet (\x ->  f x /\ g x) u

complement :: (ResiduatedLattice l, Num l) => LSet a l -> LSet a l
complement (LSet f u) = LSet (negation . f)  u

setTnorm :: (ResiduatedLattice l) => LSet a l -> LSet a l -> LSet a l
setTnorm (LSet f u) (LSet g _) = LSet (\x -> f x `tnorm` g x) u

setResiduum :: (ResiduatedLattice l) => LSet a l -> LSet a l -> LSet a l
setResiduum (LSet f u) (LSet g _) = LSet (\x -> f x --> g x) u

alphaCut :: (ResiduatedLattice l) => l -> LSet a l -> [a]
alphaCut alpha (LSet f u) = [x | x <- u, f x >= alpha]