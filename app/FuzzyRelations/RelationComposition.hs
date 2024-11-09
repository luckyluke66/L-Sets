module FuzzyRelations.RelationComposition (
    circlet,
    subproduct,
    superproduct, 
    square,
) where

import FuzzyRelations.FuzzyRelation (FuzzyRelation (FuzzyRelation))
import Lattices.CompleteResiduatedLattice 


circlet :: (CompleteResiduatedLattice l) =>  FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
circlet = composition bot (\/) tnorm

subproduct :: (CompleteResiduatedLattice l) => FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
subproduct = composition top (/\) (-->)

superproduct :: (CompleteResiduatedLattice l) => FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
superproduct = composition top (/\) (<--)

square :: (CompleteResiduatedLattice l) => FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
square = composition top (/\) (<-->)

composition :: (CompleteResiduatedLattice l) => l -> (l -> l -> l) -> (l -> l -> l) -> FuzzyRelation a l -> FuzzyRelation a l -> FuzzyRelation a l
composition unit latOp op (FuzzyRelation r universe) (FuzzyRelation s _) = FuzzyRelation composed universe
    where composed x z = foldr latOp unit [r x y `op` s y z | y <- universe]