{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Fuzzy.Relations.LRelation (
    LRelation(LRelation),
    FuzzySet(..),
) where

import Lattices.ResiduatedLattice
import Data.List
import Data.Maybe
import FuzzySet


-- | Binary L relation is a fuzzy set on a universe of pairs
data (ResiduatedLattice l, Eq a) => LRelation a l = LRelation
    { membership :: (a, a) -> l
    , universe :: ![(a, a)]
    }
    -- normally fuzzy relation is function R: X x Y -> L va 
    -- but we can create any type U =  X | Y
    -- this way we can represent the relation with one universal set
    -- so we have R: U x U -> L

instance (Eq a, Show a, Show l, ResiduatedLattice l) => Show (LRelation a l) where
    show :: LRelation a l -> String
    show (LRelation r u) =
        let memberships = [(p, r p) | p <- u]
        in  "LRelation {\n"
        ++ "  Memberships: " ++ show memberships ++ "\n"
        ++ "\n}"

instance (Eq a, ResiduatedLattice l) => FuzzySet (LRelation a l) (a, a) l where
    member :: LRelation a l -> (a, a) -> l
    member (LRelation r _) = r
    universe :: LRelation a l -> [(a, a)]
    universe (LRelation _ u) = u
    mkFuzzySet :: ((a, a) -> l) -> [(a, a)] -> LRelation a l
    mkFuzzySet = LRelation
