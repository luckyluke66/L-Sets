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
    mkUniversalRel, 
    toList
) where

import Lattices.ResiduatedLattice
import Data.List
import Data.Maybe
import FuzzySet
import Utils.Utils (universeToList, listToUniverse)


{- | Binary L relation is a fuzzy set on a universe of pairs -}
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


{- | Construct a fuzzy relation from a fuzzy set

==== __Examples__

>>> let fuzzySet = fromPairs [((1, 2), 0.5), ((2, 3), 0.8)] :: LSet (Int, Int) UILukasiewicz
>>> let rel = fromFuzzySet fuzzySet
>>> rel
"LRelation {Memberships: [((1,2),0.5),((2,3),0.8)]}"
-}
fromFuzzySet :: (FuzzySet f (a, a) l, ResiduatedLattice l, Eq a) => f -> LRelation a l
fromFuzzySet fuzzySet = LRelation (member fuzzySet) (FuzzySet.universe fuzzySet)


{- | Construct a fuzzy relation from a list of pairs-}
fromList :: (ResiduatedLattice l, Eq a) => [((a, a), l)] -> LRelation a l
fromList lst = LRelation member (listToUniverse u)
    where
        member (x, y) = fromMaybe bot (lookup (x, y) lst)
        u = universeToList (map fst lst)


{- | Construct a fuzzy relation from a membership function and a universe

==== __Examples__

>>> let f (x, y) = if x < y then 0.7 else 0.3
>>> let rel = fromFunction f [(1, 2), (2, 3), (3, 1)] :: LRelation Int UILukasiewicz
>>> toPairs rel
[((1,2),0.7),((2,3),0.7),((3,1),0.3)]
-}
fromFunction :: (ResiduatedLattice l, Eq a) => ((a, a) -> l) -> [a] -> LRelation a l 
fromFunction f u = LRelation f (listToUniverse u)


{- | Construct an empty fuzzy relation

==== __Examples__

>>> let emptyRel = mkEmptyRel :: LRelation Int UILukasiewicz
>>> toPairs emptyRel
[]
-}
mkEmptyRel :: (ResiduatedLattice l, Eq a) => LRelation a l
mkEmptyRel = LRelation (const bot) []


{- | Construct a singleton fuzzy relation

==== __Examples__

>>> let singletonRel = mkSingletonRel [(1, 2), (2, 3)] ((1, 2), 0.8) :: LRelation Int UILukasiewicz
>>> toPairs singletonRel
[((1, 2), 0.8),((2, 3), 0.0)]
-}
mkSingletonRel :: (ResiduatedLattice l, Eq a) => [a] -> ((a, a), l) -> LRelation a l
mkSingletonRel u (x, l) = LRelation f (listToUniverse u)
    where f pair = if pair == x then l else bot


{- | Construct a universal fuzzy relation

==== __Examples__

>>> let universalRel = mkUniversalRel [(1, 2), (2, 3)] :: LRelation Int UILukasiewicz
>>> toPairs universalRel
[((1, 2), 1.0),((2, 3), 1.0)]
-}
mkUniversalRel :: (ResiduatedLattice l, Eq a) => [a] -> LRelation a l
mkUniversalRel u = LRelation (const top) (listToUniverse u)


-- | Return relation as a list of pairs
toList :: (ResiduatedLattice l, Eq a) => LRelation a l -> [((a, a), l)]
toList (LRelation f u) = [(x, f x) | x <- u]
