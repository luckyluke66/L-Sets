{- | This module contains parametrized membership function for fuzzy relations
    use currying to construct the functions 
    arguments a b c ... are parameters for constructing specific functions 
    (x, y) is pair for which membership is evaluated
-}
module Fuzzy.Relations.MembershipFunctions (
    isCloseTo
) where

import Lattices.ResiduatedLattice

-- | fuzzy relation representing closeness of two numbers
isCloseTo :: ResiduatedLattice l => (Double, Double) -> l
isCloseTo (x, y) = mkLattice $ max 0 (1 - abs (x - y))
