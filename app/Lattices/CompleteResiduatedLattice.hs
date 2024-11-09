module Lattices.CompleteResiduatedLattice(
    CompleteResiduatedLattice(..),
    BoundedLattice(..),
) where


type Nat = Int

class BoundedLattice l where
    (/\), (\/) :: l -> l -> l
    top, bot :: l
    mkLattice :: Double -> l

class BoundedLattice l => CompleteResiduatedLattice l where
    (-->), (<--), tnorm, (<-->) :: l -> l -> l
    a <--> b = a --> b /\ b --> a
    negation :: l -> l
    negation a = a --> bot
    
    power :: l -> Nat -> l
    power a 0 = top
    power a n = a /\ power a (n - 1)
    a <-- b = b --> a
