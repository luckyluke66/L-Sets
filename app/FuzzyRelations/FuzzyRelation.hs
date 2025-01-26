{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module FuzzyRelations.FuzzyRelation (
    FuzzyRelation(FuzzyRelation),
    LSet(..),
) where

import Lattices.ResiduatedLattice
import Data.List
import Data.Maybe
import LSet



data (ResiduatedLattice l, Eq a) => FuzzyRelation a l  =  FuzzyRelation
    { membershipFunction :: (a, a) -> l
    , universeSet :: ![(a, a)]
    }
    -- normally fuzzy relation is function R: X x Y -> L va 
    -- but we can create any type U =  X | Y
    -- this way we can represent the relation with one universal set
    -- so we have R: U x U -> L

instance (Eq a, Show a, Show l, ResiduatedLattice l) => Show (FuzzyRelation a l) where
  show (FuzzyRelation r u) =
    let memberships = [(p, r p) | p <- u]
    in  "FuzzyRelation {\n"
        ++ "  Memberships: " ++ show memberships ++ "\n"
        ++ "\n}"

instance (Eq a, ResiduatedLattice l) => LSet (FuzzyRelation a l) (a, a) l where
    member :: ResiduatedLattice l => FuzzyRelation a l -> (a, a) -> l
    member (FuzzyRelation r _) = r
    universe :: ResiduatedLattice l => FuzzyRelation a l -> [(a, a)]
    universe (FuzzyRelation _ u) = u



--alphaCut :: (ResiduatedLattice l) => l -> FuzzyRelation a l -> [(a, a)]
--alphaCut alpha (FuzzyRelation f u) = [(x, y) | x <- u, y <- u, f (x, y) >= alpha]
