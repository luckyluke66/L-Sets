module FuzzyRelations.Properties (
    ref,
    sym,
    tra,
    antiSym,
    irref,
    asym,
) where

import FuzzyRelations.FuzzyRelation
import Lattices.ResiduatedLattice

--functions determining degree of properties of fuzzy relations

ref :: (ResiduatedLattice l) => FuzzyRelation a l -> l
ref (FuzzyRelation f universe _) =
    foldr (/\) bot [f (x, x) | x <- universe]

sym :: (ResiduatedLattice l) => FuzzyRelation a l -> l
sym (FuzzyRelation f universe _) =
    foldr (/\) bot [f (x, y) --> f (y, x) | x <- universe, y <- universe]

tra :: (ResiduatedLattice l) => FuzzyRelation a l -> l
tra (FuzzyRelation f universe _) =
    foldr (/\) bot [f (x, y) /\ f (y, z) --> f (x, z) | x <- universe, y <- universe, z <- universe]

antiSym :: (ResiduatedLattice l) => FuzzyRelation a l -> l
antiSym (FuzzyRelation f universe eq) =
    foldr (/\) bot [f (x, y) /\ f (y, x) -->  eq x y | x <- universe, y <- universe]

irref :: (ResiduatedLattice l) => FuzzyRelation a l -> l
irref (FuzzyRelation f universe _) = negation $
    foldr (/\) top [f (x, x) | x <- universe]

asym :: (ResiduatedLattice l) => FuzzyRelation a l -> l
asym (FuzzyRelation f universe _) =
    foldr (/\) bot [f (x, y) --> negation (f (y, x)) | x <- universe, y <- universe]
