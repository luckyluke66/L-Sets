{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lattices.UnitIntervalStructures.Product(UIProduct(UIProduct)) where

import Lattices.ResiduatedLattice
import Lattices.UnitInterval

newtype UIProduct = UIProduct UnitInterval 
    deriving (Eq, Ord, Num, Real, RealFrac, Fractional)

instance BoundedLattice UIProduct where
    (/\) (UIProduct x) (UIProduct y) = UIProduct (x /\ y)
    (\/) (UIProduct x) (UIProduct y) = UIProduct (x \/ y)
    bot = UIProduct bot
    top = UIProduct top
    mkLattice x
        | x <= 0 = bot
        | x >= 1 = top
        | otherwise = UIProduct (mkUnitInterval x)

-- | Product (Goguen) structure of truth values
instance ResiduatedLattice UIProduct where
    tnorm x y = x * y
    a --> b = if a <= b then top else a / b 

instance Show UIProduct where 
    show (UIProduct x) = show x