module Lattices.ResiduatedLattice(
    ResiduatedLattice(..),
    BoundedLattice(..),
) where


type Nat = Int

class (Eq l, Ord l, Num l, Real l, RealFrac l, Fractional l) => BoundedLattice l where
    (/\), (\/) :: l -> l -> l
    top, bot :: l
    mkLattice :: Double -> l
    -- proc jsou tam vsechny classy? 

class BoundedLattice l => ResiduatedLattice l where
    (-->), (<--), tnorm, (<-->) :: l -> l -> l
    a <--> b = a --> b /\ b --> a
    negation :: l -> l
    negation a = a --> bot
    
    power :: l -> Nat -> l
    power a 0 = top
    power a n = a /\ power a (n - 1) -- je to spravne /\?
    a <-- b = b --> a
