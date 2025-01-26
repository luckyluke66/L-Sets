module FuzzyRelations.RelationComposition (
    circlet,
    subproduct,
    superproduct,
    square,
) where

import FuzzyRelations.FuzzyRelation (FuzzyRelation (FuzzyRelation))
import Lattices.ResiduatedLattice
import Utils.Utils


circlet :: (Eq a,ResiduatedLattice l) =>  FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
circlet (FuzzyRelation r u) (FuzzyRelation s _ ) = FuzzyRelation composition u
    where composition (x, z) = foldr (\/) bot [r (x, y) `tnorm` s (y, z) | y <- universe]
          universe = universeToList u


subproduct :: (Eq a,ResiduatedLattice l) => FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
subproduct (FuzzyRelation r u) (FuzzyRelation s _) = FuzzyRelation composition u
    where composition (x, z) = foldr (/\) top [r (x, y) --> s (y, z) | y <- universe]
          universe = universeToList u
        
superproduct :: (Eq a,ResiduatedLattice l) => FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
superproduct (FuzzyRelation r u) (FuzzyRelation s _) = FuzzyRelation composition u
    where composition (x, z) = foldr (/\) bot [r (x, y) <-- s (y, z) | y <- universe]
          eq x y = r (x, y) <--> s (x, y)
          universe = universeToList u

square :: (Eq a, ResiduatedLattice l) => FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
square (FuzzyRelation r u) (FuzzyRelation s _) = FuzzyRelation composition u
    where composition (x, z) = foldr (/\) bot [r (x, y) <--> s (y, z) | y <- universe]
          universe = universeToList u

