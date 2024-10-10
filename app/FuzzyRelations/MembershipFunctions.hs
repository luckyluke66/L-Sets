module FuzzyRelations.MembershipFunctions (
    isCloseTo
) where

import Lattices.CompleteResiduatedLattice

isCloseTo :: CompleteResiduatedLattice l => (Double, Double) -> l
isCloseTo (x, y) = mkLattice $ max 0 (1 - abs (x - y))
