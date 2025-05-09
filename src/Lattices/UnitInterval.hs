{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lattices.UnitInterval(
    UnitInterval(..),
    mkUnitInterval
) where

import Lattices.ResiduatedLattice
import Data.Ord (clamp)

-- | unit interval on real numbers [0,1]
newtype UnitInterval = UnitInterval Double 
    deriving (Eq, Ord, Num, Real, RealFrac, Fractional)

instance BoundedLattice UnitInterval where
    (/\) = min
    (\/) = max
    top = 1
    bot = 0
    mkLattice = mkUnitInterval

instance Show UnitInterval where
    show (UnitInterval x) = show x

-- | Unit interval constructor. Ensures values are in bounds
mkUnitInterval :: Double -> UnitInterval
mkUnitInterval x = UnitInterval $ clamp (0, 1) x