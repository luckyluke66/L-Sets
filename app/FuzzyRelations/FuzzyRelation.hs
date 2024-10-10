module FuzzyRelations.FuzzyRelation (
    FuzzyRelation,
) where

import Lattices.CompleteResiduatedLattice 
import FuzzySets.FuzzySet

type FuzzyRelation a l = FuzzySet (a, a) l
