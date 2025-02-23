module Fuzzy.Relations.MembershipFunctions (
    isCloseTo
) where

import Lattices.ResiduatedLattice

isCloseTo :: ResiduatedLattice l => (Double, Double) -> l
isCloseTo (x, y) = mkLattice $ max 0 (1 - abs (x - y))
