module Lattices.UnitInterval(
    UnitInterval,
    mkUnitInterval
) where

import Lattices.CompleteResiduatedLattice

newtype UnitInterval = UnitInterval Double 
    deriving (Eq, Ord)

mkUnitInterval :: Double -> UnitInterval
mkUnitInterval x = UnitInterval $ max 0 $ min 1 x

instance Num UnitInterval where
    (UnitInterval a) + (UnitInterval b) = UnitInterval $ a + b
    (UnitInterval a) - (UnitInterval b) = UnitInterval $ a - b
    (UnitInterval a) * (UnitInterval b) = UnitInterval $ a * b
    abs (UnitInterval a) = UnitInterval $ abs a
    signum (UnitInterval a) = UnitInterval $ signum a
    fromInteger a = UnitInterval $ fromInteger a

instance CompleteResiduatedLattice UnitInterval where
    (/\) = min
    (\/) = max
    residuum a b = min (1 - a + b) 1
    tnorm a b = max (a + b - 1) 0
    inf = 0
    sup = 1

instance Show UnitInterval where
    show (UnitInterval x) = show x