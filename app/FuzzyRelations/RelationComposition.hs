module FuzzyRelations.RelationComposition (
    circlet,
    subproduct,
    superproduct, 
    square,
) where

import FuzzyRelations.FuzzyRelation (FuzzyRelation (FuzzyRelation))
import Lattices.ResiduatedLattice


circlet :: (ResiduatedLattice l) =>  FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
circlet = composition bot (\/) tnorm

subproduct :: (ResiduatedLattice l) => FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
subproduct = composition top (/\) (-->)

superproduct :: (ResiduatedLattice l) => FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
superproduct = composition top (/\) (<--)

square :: (ResiduatedLattice l) => FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
square = composition top (/\) (<-->)

composition :: (ResiduatedLattice l) => l -> (l -> l -> l) -> (l -> l -> l) -> FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
composition unit latOp op (FuzzyRelation r universe eq) (FuzzyRelation s _ _) = FuzzyRelation composed universe eq
    where composed (x, z) = foldr latOp unit [r (x, y) `op` s (y, z) | y <- universe]