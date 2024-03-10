module Lattices.BooleanAlgebra where

import Lattices.CompleteResiduatedLattice

instance CompleteResiduatedLattice Bool where
    (/\) = min
    (\/) = max
    inf = False
    sup = True
    residuum = implication 
    tnorm a b = a && b

implication :: Bool -> Bool -> Bool
implication True False = False
implication _ _        = True