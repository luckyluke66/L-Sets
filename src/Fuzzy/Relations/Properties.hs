-- | This module contains functions that determine degree of properties of a 'LRelation'
module Fuzzy.Relations.Properties (
    ref,
    sym,
    tra,
    irref,
    asym,
) where

import Fuzzy.Relations.LRelation
import Lattices.ResiduatedLattice
import Utils.Utils(universeToList)

-- functions determining degree of properties of fuzzy relations

-- | Degree to which 'LRelation' is reflexive 
ref :: (Eq a, ResiduatedLattice l) => LRelation a l -> l
ref (LRelation f u) =
    foldr (/\) top [f (x, x) | x <- universe]
    where universe = universeToList u

-- | Degree to which 'LRelation' is symmetric
sym :: (Eq a,ResiduatedLattice l) => LRelation a l -> l
sym (LRelation f u) =
    foldr (/\) top [f (x, y) --> f (y, x) | x <- universe, y <- universe]
    where universe = universeToList u

-- | Degree to which 'LRelation' is transitive
tra :: (Eq a,ResiduatedLattice l) => LRelation a l -> l
tra (LRelation f u) =
    foldr (/\) top [f (x, y) /\ f (y, z) --> f (x, z) | x <- universe, y <- universe, z <- universe]
    where universe = universeToList u

-- | Degree to which 'LRelation' is irreflexive
irref :: (Eq a,ResiduatedLattice l) => LRelation a l -> l
irref (LRelation f u) = negation $
    foldr (/\) top [f (x, x) | x <- universe]
    where universe = universeToList u

-- | Degree to which 'LRelation' is asymmetric
asym :: (Eq a,ResiduatedLattice l) => LRelation a l -> l
asym (LRelation f u) =
    foldr (/\) top [f (x, y) --> negation (f (y, x)) | x <- universe, y <- universe]
    where universe = universeToList u
