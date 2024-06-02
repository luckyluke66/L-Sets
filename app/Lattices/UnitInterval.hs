module Lattices.UnitInterval(
    UnitInterval,
    mkUnitInterval
) where

import Lattices.CompleteResiduatedLattice

newtype UnitInterval = UnitInterval Double 
    deriving (Eq, Ord)

instance Num UnitInterval where
    (UnitInterval a) + (UnitInterval b) = UnitInterval $ a + b
    (UnitInterval a) - (UnitInterval b) = UnitInterval $ a - b
    (UnitInterval a) * (UnitInterval b) = UnitInterval $ a * b
    abs (UnitInterval a) = UnitInterval $ abs a
    signum (UnitInterval a) = UnitInterval $ signum a
    fromInteger a = UnitInterval $ fromInteger a

instance Fractional UnitInterval where
    (UnitInterval a) / (UnitInterval b) = UnitInterval $ a / b
    fromRational a = UnitInterval $ fromRational a

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