{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Lattices.UnitIntervalStructures.Godel(
    UIGodel(UIGodel),
    BoundedLattice(..),
    mkGodelUnitInterval,
    fromGodelUnitInterval
) where

import Lattices.ResiduatedLattice
import Lattices.UnitInterval

newtype UIGodel = UIGodel UnitInterval 
    deriving (Eq, Ord, Num, Real, RealFrac, Fractional)

instance BoundedLattice UIGodel where
    (/\) :: UIGodel -> UIGodel -> UIGodel
    (/\) (UIGodel x) (UIGodel y) = UIGodel (x /\ y)
    (\/) (UIGodel x) (UIGodel y) = UIGodel (x \/ y)
    bot = UIGodel bot
    top = UIGodel top
    mkLattice = mkGodelUnitInterval

-- | Gödel structure of truth values
instance ResiduatedLattice UIGodel where
    tnorm x y = x /\ y
    (-->)  = godelResiduum

instance Show UIGodel where
    show (UIGodel x) = show x

mkGodelUnitInterval :: Double -> UIGodel
mkGodelUnitInterval x = UIGodel $ mkUnitInterval x

fromGodelUnitInterval :: UIGodel -> Double
fromGodelUnitInterval (UIGodel (UnitInterval x)) = x

godelResiduum :: UIGodel -> UIGodel -> UIGodel
godelResiduum x y
    | x <= y = top
    | otherwise = y