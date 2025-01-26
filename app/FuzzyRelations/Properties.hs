module FuzzyRelations.Properties (
    ref,
    sym,
    tra,
    irref,
    asym,
) where

import FuzzyRelations.FuzzyRelation
import Lattices.ResiduatedLattice
import Utils.Utils(universeToList)

--functions determining degree of properties of fuzzy relations

ref :: (Eq a, ResiduatedLattice l) => FuzzyRelation a l -> l
ref (FuzzyRelation f u) =
    foldr (/\) bot [f (x, x) | x <- universe]
    where universe = universeToList u

sym :: (Eq a,ResiduatedLattice l) => FuzzyRelation a l -> l
sym (FuzzyRelation f u) =
    foldr (/\) bot [f (x, y) --> f (y, x) | x <- universe, y <- universe]
    where universe = universeToList u

tra :: (Eq a,ResiduatedLattice l) => FuzzyRelation a l -> l
tra (FuzzyRelation f u) =
    foldr (/\) bot [f (x, y) /\ f (y, z) --> f (x, z) | x <- universe, y <- universe, z <- universe]
    where universe = universeToList u

irref :: (Eq a,ResiduatedLattice l) => FuzzyRelation a l -> l
irref (FuzzyRelation f u) = negation $
    foldr (/\) top [f (x, x) | x <- universe]
    where universe = universeToList u

asym :: (Eq a,ResiduatedLattice l) => FuzzyRelation a l -> l
asym (FuzzyRelation f u) =
    foldr (/\) bot [f (x, y) --> negation (f (y, x)) | x <- universe, y <- universe]
    where universe = universeToList u
