module Lattices.UnitIntervalStructures.Lukasiewicz(UILukasiewicz(UILukasiewicz)) where

import Lattices.CompleteResiduatedLattice
import Lattices.UnitInterval

newtype UILukasiewicz = UILukasiewicz UnitInterval 
    deriving (Eq, Ord)

instance BoundedLattice UILukasiewicz where
    (/\) (UILukasiewicz x) (UILukasiewicz y) = UILukasiewicz (x /\ y)
    (\/) (UILukasiewicz x) (UILukasiewicz y) = UILukasiewicz (x \/ y)
    bot = UILukasiewicz bot
    top = UILukasiewicz top
    mkLattice x
        | x <= 0 = bot
        | x >= 1 = top
        | otherwise = UILukasiewicz (mkUnitInterval x)

instance CompleteResiduatedLattice UILukasiewicz where
    tnorm a b = a + b - top \/ bot
    a --> b = top - a + b /\ top

instance Num UILukasiewicz where
    (UILukasiewicz x) + (UILukasiewicz y) = UILukasiewicz (x + y)
    (UILukasiewicz x) * (UILukasiewicz y) = UILukasiewicz (x * y)
    abs (UILukasiewicz x) = UILukasiewicz (abs x)
    signum (UILukasiewicz x) = UILukasiewicz (signum x)
    fromInteger x = UILukasiewicz (fromInteger x)
    negate (UILukasiewicz x) = UILukasiewicz (negate x)

instance Show UILukasiewicz where 
    show (UILukasiewicz x) = show x