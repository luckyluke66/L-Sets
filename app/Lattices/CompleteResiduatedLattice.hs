module Lattices.CompleteResiduatedLattice(
    CompleteResiduatedLattice(..)
) where 

class CompleteResiduatedLattice l where
    (/\), (\/), residuum, tnorm :: l -> l -> l
    inf, sup :: l