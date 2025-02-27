{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lattices.UnitIntervalStructures.Lukasiewicz(
    UILukasiewicz(UILukasiewicz),
    mkLukasiewiczUnitInterval
) where

import Lattices.ResiduatedLattice
import Lattices.UnitInterval

newtype UILukasiewicz = UILukasiewicz UnitInterval 
    deriving (Eq, Ord, Num, Real, RealFrac, Fractional)

instance BoundedLattice UILukasiewicz where
    (/\) (UILukasiewicz x) (UILukasiewicz y) = UILukasiewicz (x /\ y)
    (\/) (UILukasiewicz x) (UILukasiewicz y) = UILukasiewicz (x \/ y)
    bot = UILukasiewicz bot
    top = UILukasiewicz top
    mkLattice = mkLukasiewiczUnitInterval

-- | Łukasiewicz structure of truth values
instance ResiduatedLattice UILukasiewicz where
    tnorm a b = a + b - top \/ bot
    a --> b = top - a + b /\ top

instance Show UILukasiewicz where 
    show (UILukasiewicz x) = show x

mkLukasiewiczUnitInterval :: Double -> UILukasiewicz
mkLukasiewiczUnitInterval x = UILukasiewicz $ mkUnitInterval x