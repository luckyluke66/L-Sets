module FuzzySets.FuzzySet(
    FuzzySet(FuzzySet),
    membership,
    crop
    ) where

import Lattices.UnitInterval
import Lattices.CompleteResiduatedLattice

newtype (CompleteResiduatedLattice l) => FuzzySet a l = FuzzySet (a -> l)
  
membership :: (CompleteResiduatedLattice l) => FuzzySet a l -> a -> l
membership (FuzzySet memFun) = memFun

crop :: RealFloat a => a -> a
crop x = fromInteger (round (x * (10^6))) / (10.0^^6)

union :: (RealFloat a, CompleteResiduatedLattice l) => FuzzySet a l -> FuzzySet a l -> FuzzySet a l
union (FuzzySet f) (FuzzySet g) = FuzzySet (\x -> f x \/ g x)

intersection :: (RealFloat a, CompleteResiduatedLattice l) => FuzzySet a l -> FuzzySet a l -> FuzzySet a l
intersection (FuzzySet f) (FuzzySet g) = FuzzySet (\x ->  f x /\ g x)

complement :: (RealFloat a, CompleteResiduatedLattice l, Num l) => FuzzySet a l -> FuzzySet a l
complement (FuzzySet f) = FuzzySet (\x -> 1 - f x)

gradedSubsethood :: CompleteResiduatedLattice l => [a] -> FuzzySet a l -> FuzzySet a l -> l
gradedSubsethood = gradedOperation (-->)

gradedEqality :: CompleteResiduatedLattice l => [a] -> FuzzySet a l -> FuzzySet a l -> l
gradedEqality = gradedOperation (<-->)

gradedOperation :: CompleteResiduatedLattice l => (l -> l -> l) -> [a] -> FuzzySet a l -> FuzzySet a l -> l
gradedOperation op interval (FuzzySet f) (FuzzySet g) = 
    foldr (/\) bot $ zipWith op (map f interval) (map g interval)