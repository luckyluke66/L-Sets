module Lattices.UnitIntervalStructures.Product(UIProduct(UIProduct)) where

import Lattices.CompleteResiduatedLattice
import Lattices.UnitInterval

newtype UIProduct = UIProduct UnitInterval 
    deriving (Eq, Ord)

instance BoundedLattice UIProduct where
    (/\) (UIProduct x) (UIProduct y) = UIProduct (x /\ y)
    (\/) (UIProduct x) (UIProduct y) = UIProduct (x \/ y)
    bot = UIProduct bot
    top = UIProduct top
    mkLattice x
        | x <= 0 = bot
        | x >= 1 = top
        | otherwise = UIProduct (mkUnitInterval x)

instance CompleteResiduatedLattice UIProduct where
    tnorm x y = x * y
    a --> b = if a <= b then top else a / b 

instance Num UIProduct where
    (UIProduct x) + (UIProduct y) = UIProduct (x + y)
    (UIProduct x) * (UIProduct y) = UIProduct (x * y)
    abs (UIProduct x) = UIProduct (abs x)
    signum (UIProduct x) = UIProduct (signum x)
    fromInteger x = UIProduct (fromInteger x)
    negate (UIProduct x) = UIProduct (negate x)

instance Fractional UIProduct where
    fromRational x = UIProduct (fromRational x)
    recip (UIProduct x) = UIProduct (recip x)

instance Show UIProduct where 
    show (UIProduct x) = show x