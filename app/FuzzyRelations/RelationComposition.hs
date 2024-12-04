module FuzzyRelations.RelationComposition (
    circlet,
    subproduct,
    superproduct,
    square,
) where

import FuzzyRelations.FuzzyRelation (FuzzyRelation (FuzzyRelation))
import Lattices.ResiduatedLattice


circlet :: (ResiduatedLattice l) =>  FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
circlet (FuzzyRelation r universe) (FuzzyRelation s _ ) = FuzzyRelation composition universe
    where composition (x, z) = foldr (\/) bot [r (x, y) `tnorm` s (y, z) | y <- universe ]


subproduct :: (ResiduatedLattice l) => FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
subproduct (FuzzyRelation r universe) (FuzzyRelation s _) = FuzzyRelation composition universe
    where composition (x, z) = foldr (/\) top [r (x, y) --> s (y, z) | y <- universe]

superproduct :: (ResiduatedLattice l) => FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
superproduct (FuzzyRelation r universe) (FuzzyRelation s _) = FuzzyRelation composition universe
    where composition (x, z) = foldr (/\) bot [r (x, y) <-- s (y, z) | y <- universe ]
          eq x y = r (x, y) <--> s (x, y)

square :: (ResiduatedLattice l) => FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
square (FuzzyRelation r universe) (FuzzyRelation s _) = FuzzyRelation composition universe
    where composition (x, z) = foldr (/\) bot [r (x, y) <--> s (y, z) | y <- universe ]

