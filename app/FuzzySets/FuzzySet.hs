module FuzzySets.FuzzySet(
    FuzzySet(FuzzySet),
    membership,
    crop,
    union, 
    intersection,
    complement,
    gradedSubsethood,
    gradedEquality,
    ) where

import Lattices.UnitInterval
import Lattices.CompleteResiduatedLattice

data (CompleteResiduatedLattice l) => FuzzySet a l = FuzzySet
    { membershipFunction :: a -> l
    , universe :: ![a]
    }


membership :: (CompleteResiduatedLattice l) => FuzzySet a l -> a -> l
membership (FuzzySet f _) = f


crop :: RealFloat a => a -> a
crop x = fromInteger (round (x * (10^6))) / (10.0^^6)


union :: (RealFloat a, CompleteResiduatedLattice l) => FuzzySet a l -> FuzzySet a l -> FuzzySet a l
union (FuzzySet f u) (FuzzySet g _) = FuzzySet (\x -> f x \/ g x) u

intersection :: (RealFloat a, CompleteResiduatedLattice l) => FuzzySet a l -> FuzzySet a l -> FuzzySet a l
intersection (FuzzySet f u) (FuzzySet g _) = FuzzySet (\x ->  f x /\ g x) u

complement :: (RealFloat a, CompleteResiduatedLattice l, Num l) => FuzzySet a l -> FuzzySet a l
complement (FuzzySet f u) = FuzzySet (\x -> 1 - f x) u

gradedSubsethood :: CompleteResiduatedLattice l => FuzzySet a l -> FuzzySet a l -> l
gradedSubsethood = gradedOperation (-->)

gradedEquality :: CompleteResiduatedLattice l => FuzzySet a l -> FuzzySet a l -> l
gradedEquality = gradedOperation (<-->)

gradedOperation :: CompleteResiduatedLattice l => (l -> l -> l) -> FuzzySet a l -> FuzzySet a l -> l
gradedOperation op (FuzzySet f u) (FuzzySet g _) = 
    foldr (/\) bot $ zipWith op (map f u) (map g u)
