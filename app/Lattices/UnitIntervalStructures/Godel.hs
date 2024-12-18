{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lattices.UnitIntervalStructures.Godel(UIGodel(UIGodel)) where

import Lattices.ResiduatedLattice
import Lattices.UnitInterval

newtype UIGodel = UIGodel UnitInterval 
    deriving (Eq, Ord, Num, Real, RealFrac, Fractional)

instance BoundedLattice UIGodel where
    (/\) (UIGodel x) (UIGodel y) = UIGodel (x /\ y)
    (\/) (UIGodel x) (UIGodel y) = UIGodel (x \/ y)
    bot = UIGodel bot
    top = UIGodel top
    mkLattice x
        | x <= 0 = bot
        | x >= 1 = top
        | otherwise = UIGodel (mkUnitInterval x)

instance ResiduatedLattice UIGodel where
    tnorm x y = x /\ y
    (-->)  = godelResiduum

instance Show UIGodel where
    show (UIGodel x) = show x

godelResiduum :: UIGodel -> UIGodel -> UIGodel
godelResiduum x y
    | x <= y = top
    |otherwise = y