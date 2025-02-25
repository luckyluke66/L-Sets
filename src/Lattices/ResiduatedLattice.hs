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
    -- | also called upper bound
    top :: l 
    -- | also called lower bound
    bot :: l
    -- | constructor for lattice
    mkLattice :: Double -> l
    -- proc jsou tam vsechny classy? 

-- | A bounded lattice with additional laws and operations namely --> and tnorm
-- /Adjointness/
--
-- /Commutativity/
--
-- @
-- x 'tnorm' y ≡ y 'tnorm' x
-- @
--
-- /Asociativity of 'tnorm'/
--
-- @
-- x 'tnorm' (y 'tnorm' z) ≡ (x 'tnorm' y) 'tnorm' z
-- @
--
-- /Identity Element/
-- @
--  x 'tnorm' 1 ≡ x
-- @
--
-- @
-- x ≤ y '-->' z iff x 'tnorm' y ≤ z 
-- @
class BoundedLattice l => ResiduatedLattice l where
    -- | residuum
    (-->), (<--) :: l -> l -> l 
    a <-- b = b --> a
    -- | biresiduum 
    (<-->) :: l -> l -> l
    a <--> b = a --> b /\ b --> a
    tnorm :: l -> l -> l 
    negation :: l -> l
    negation a = a --> bot
    
    power :: l -> Nat -> l
    power a 0 = top
    power a n = a /\ power a (n - 1) -- je to spravne /\?