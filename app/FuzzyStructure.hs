module FuzzyStructure where
import Lattices.ResiduatedLattice

class FuzzyStructure struct where
    membership ::(ResiduatedLattice l) =>  struct a l -> a -> l
    universe ::(ResiduatedLattice l) =>  struct a l -> [a]

class FuzzyStructure struct => FuzzyRelationClass struct where
    eq :: (ResiduatedLattice l) => struct a l -> a -> a -> l