{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Fuzzy.Relations.LRelation (
    LRelation(LRelation),
    FuzzySet(..),
    fromList,
    fromFuzzySet,
    fromFunction,
    mkEmptyRel,
    mkSingletonRel,
    mkUniversalRel
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


fromFuzzySet :: (FuzzySet f (a, a) l, ResiduatedLattice l, Eq a) => f -> LRelation a l
fromFuzzySet fuzzySet = LRelation (member fuzzySet) (FuzzySet.universe fuzzySet)


fromList :: (ResiduatedLattice l, Eq a) => [((a, a), l)] -> LRelation a l
fromList lst = LRelation member u
    where
        member (x, y) = fromMaybe bot (lookup (x, y) lst)
        u = map fst lst


fromFunction :: (ResiduatedLattice l, Eq a) => ((a, a) -> l) -> [(a, a)] -> LRelation a l 
fromFunction = LRelation


mkEmptyRel :: (ResiduatedLattice l, Eq a) => LRelation a l
mkEmptyRel = LRelation (const bot) []


-- | construct a singleton fuzzy set
mkSingletonRel :: (ResiduatedLattice l, Eq a) => [(a, a)] -> ((a, a), l) -> LRelation a l
mkSingletonRel u (x, l) = LRelation f u
    where f pair = if pair == x then l else bot


-- | construct universal fuzzy set
mkUniversalRel :: (ResiduatedLattice l, Eq a) => [(a, a)] -> LRelation a l
mkUniversalRel = LRelation (const top)