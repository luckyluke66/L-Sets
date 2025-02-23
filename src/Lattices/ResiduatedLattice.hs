module Lattices.ResiduatedLattice(
    ResiduatedLattice(..),
    BoundedLattice(..),
    Nat
) where


type Nat = Int

-- | Lattice is an algebraic structure with join and meet operations.
-- BoundedLattice has Top and Bottom elements
-- Lattice satisfies following laws:
--
-- /Associativity/
--
-- @
-- x '\/' (y '\/' z) ≡ (x '\/' y) '\/' z
-- x '/\' (y '/\' z) ≡ (x '/\' y) '/\' z
-- @
--
-- /Commutativity/
--
-- @
-- x '\/' y ≡ y '\/' x
-- x '/\' y ≡ y '/\' x
-- @
--
-- /Idempotency/
--
-- @
-- x '\/' x ≡ x
-- x '/\' x ≡ x
-- @
--
-- /Absorption/
--
-- @
-- a '\/' (a '/\' b) ≡ a
-- a '/\' (a '\/' b) ≡ a
-- @
class (Eq l, Ord l, Num l, Real l, RealFrac l, Fractional l) => BoundedLattice l where
    -- | meet
    (/\) :: l -> l -> l
    -- | join
    (\/) :: l -> l -> l
    top, bot :: l
    -- | constructor for lattice
    mkLattice :: Double -> l
    -- proc jsou tam vsechny classy? 

-- 
class BoundedLattice l => ResiduatedLattice l where
    -- | residuum
    (-->), (<--) :: l -> l -> l 
    -- | biresiduum 
    (<-->) :: l -> l -> l
    a <--> b = a --> b /\ b --> a
    tnorm :: l -> l -> l 
    negation :: l -> l
    negation a = a --> bot
    
    power :: l -> Nat -> l
    power a 0 = top
    power a n = a /\ power a (n - 1) -- je to spravne /\?
    a <-- b = b --> a
