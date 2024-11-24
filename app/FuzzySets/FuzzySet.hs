module FuzzySets.FuzzySet(
    FuzzySet(FuzzySet),
    membership,
    universe,
    crop,
    union, 
    intersection,
    complement,
    gradedSubsethood,
    gradedEquality,
    alphaCut,
) where

import Lattices.UnitInterval
import Lattices.ResiduatedLattice


data (ResiduatedLattice l) => FuzzySet a l = FuzzySet
    { membershipFunction :: a -> l
    , universeSet :: ![a]
    }


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
complement (FuzzySet f u) = FuzzySet (\x -> 1 - f x) u

gradedSubsethood :: ResiduatedLattice l => FuzzySet a l -> FuzzySet a l -> l
gradedSubsethood = gradedOperation (-->)

gradedEquality :: ResiduatedLattice l => FuzzySet a l -> FuzzySet a l -> l
gradedEquality = gradedOperation (<-->)

gradedOperation :: ResiduatedLattice l => (l -> l -> l) -> FuzzySet a l -> FuzzySet a l -> l
gradedOperation op (FuzzySet f u) (FuzzySet g _) = 
    foldr (/\) bot $ zipWith op (map f u) (map g u)

alphaCut :: (ResiduatedLattice l) => l -> FuzzySet a l -> [a]
alphaCut alpha (FuzzySet f u) = [x | x <- u, f x >= alpha]