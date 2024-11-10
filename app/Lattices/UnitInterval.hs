{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lattices.UnitInterval(
    UnitInterval,
    mkUnitInterval
) where

import Lattices.CompleteResiduatedLattice

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

mkUnitInterval :: Double -> UnitInterval
mkUnitInterval x = UnitInterval $ max 0 $ min 1 x