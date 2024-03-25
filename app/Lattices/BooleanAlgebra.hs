module Lattices.BooleanAlgebra where

import Lattices.CompleteResiduatedLattice


instance BoundedLattice Bool where 
    (/\) = min
    (\/) = max
    bot = False
    top = True
    mkLattice x = if x == 0 then bot else top

instance CompleteResiduatedLattice Bool where
    (-->) = implication 
    tnorm = (&&)

implication :: Bool -> Bool -> Bool
implication True False = False
implication _ _        = True