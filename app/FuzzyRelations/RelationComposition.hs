module FuzzyRelations.RelationComposition (
    circlet,
    subproduct,
    superproduct,
    square,
) where

import FuzzyRelations.LRelation
import Lattices.ResiduatedLattice
import Utils.Utils


circlet :: (Eq a,ResiduatedLattice l) =>  LRelation a l -> LRelation a l -> LRelation a l
circlet (LRelation r u) (LRelation s _ ) = LRelation composition u
    where composition (x, z) = foldr (\/) bot [r (x, y) `tnorm` s (y, z) | y <- universe]
          universe = universeToList u


subproduct :: (Eq a,ResiduatedLattice l) => LRelation a l -> LRelation a l -> LRelation a l
subproduct (LRelation r u) (LRelation s _) = LRelation composition u
    where composition (x, z) = foldr (/\) top [r (x, y) --> s (y, z) | y <- universe]
          universe = universeToList u
        
superproduct :: (Eq a,ResiduatedLattice l) => LRelation a l -> LRelation a l -> LRelation a l
superproduct (LRelation r u) (LRelation s _) = LRelation composition u
    where composition (x, z) = foldr (/\) bot [r (x, y) <-- s (y, z) | y <- universe]
          eq x y = r (x, y) <--> s (x, y)
          universe = universeToList u

square :: (Eq a, ResiduatedLattice l) => LRelation a l -> LRelation a l -> LRelation a l
square (LRelation r u) (LRelation s _) = LRelation composition u
    where composition (x, z) = foldr (/\) bot [r (x, y) <--> s (y, z) | y <- universe]
          universe = universeToList u

