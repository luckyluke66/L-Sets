{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lattices.UnitIntervalStructures.Lukasiewicz(UILukasiewicz(UILukasiewicz)) where

import Lattices.ResiduatedLattice
import Lattices.UnitInterval

newtype UILukasiewicz = UILukasiewicz UnitInterval 
    deriving (Eq, Ord, Num, Real, RealFrac, Fractional)

instance BoundedLattice UILukasiewicz where
    (/\) (UILukasiewicz x) (UILukasiewicz y) = UILukasiewicz (x /\ y)
    (\/) (UILukasiewicz x) (UILukasiewicz y) = UILukasiewicz (x \/ y)
    bot = UILukasiewicz bot
    top = UILukasiewicz top
    mkLattice x
        | x <= 0 = bot
        | x >= 1 = top
        | otherwise = UILukasiewicz (mkUnitInterval x)

instance ResiduatedLattice UILukasiewicz where
    tnorm a b = a + b - top \/ bot
    a --> b = top - a + b /\ top

instance Show UILukasiewicz where 
    show (UILukasiewicz x) = show x