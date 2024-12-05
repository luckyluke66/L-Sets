module FuzzyStructure where

class FuzzyStructure a where
    membership :: a -> ResiduatedLattice l
    universe :: a -> [b]