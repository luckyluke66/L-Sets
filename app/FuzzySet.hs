module FuzzySet(
    FuzzySet(FuzzySet),
    membership,
    crop
    ) where

import Lattices.UnitInterval(UnitInterval, mkUnitInterval)
import Lattices.CompleteResiduatedLattice(CompleteResiduatedLattice(..))

newtype (CompleteResiduatedLattice l) => FuzzySet a l = FuzzySet (a -> l)
  
membership :: (CompleteResiduatedLattice l) => FuzzySet a l -> a -> l
membership (FuzzySet memFun) = memFun

crop :: RealFloat a => a -> a
crop x = fromInteger (round (x * (10^6))) / (10.0^^6)

union :: RealFloat a => FuzzySet a UnitInterval -> FuzzySet a UnitInterval -> FuzzySet a UnitInterval
union (FuzzySet x) (FuzzySet y) = FuzzySet (\z -> max (x z) (y z))

intersection :: RealFloat a => FuzzySet a UnitInterval -> FuzzySet a UnitInterval -> FuzzySet a UnitInterval
intersection (FuzzySet x) (FuzzySet y) = FuzzySet (\z -> min (x z) (y z))

complement :: RealFloat a => FuzzySet a UnitInterval -> FuzzySet a UnitInterval
complement (FuzzySet f) = FuzzySet (\x -> 1 - f x)