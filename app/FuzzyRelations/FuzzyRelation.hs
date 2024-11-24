module FuzzyRelations.FuzzyRelation (
    FuzzyRelation(FuzzyRelation),
    membership,
    alphaCut
) where

import Lattices.ResiduatedLattice
import Data.List
import Data.Maybe
import FuzzyStructure



data (ResiduatedLattice l) => FuzzyRelation a l  =  FuzzyRelation
    { membershipFunction :: a -> l
    , universeSet :: ![a]
    }
    -- normally fuzzy relation is function R: X x Y -> L 
    -- but we can create any type U =  X | Y
    -- this way we can represent the relation with one universal set
    -- so we have R: U x U -> L

instance (Show a, Show l, ResiduatedLattice l) => Show (FuzzyRelation a l) where
  show (FuzzyRelation mf us) =
    let pairs = [(x, y) | x <- us, y <- us] -- All pairs in the universe
        memberships = [(p, mf p) | p <- pairs] -- Membership values for each pair
    in  "FuzzyRelation {\n"
        ++ "  Memberships: " ++ show memberships ++ "\n"
        ++ "\n}"

instance FuzzyStructure FuzzyRelation where
    membership (FuzzyRelation f _ _) = f
    universe (FuzzyRelation _ u _) = u

fromTriplet :: (ResiduatedLattice l, Eq a) => [(a, a, l)] -> FuzzyRelation a l
fromTriplet triplets = FuzzyRelation f u
    where u = map (\(x, _, _) -> x) triplets
          f (x, y) = head [z | (a, b, z) <- triplets, a == x, b == y]

fromMatrix :: (ResiduatedLattice l, Eq a) => [[l]] -> [a] -> FuzzyRelation a l
fromMatrix matrix universe = FuzzyRelation f universe
    where f (x, y) = 
            let i = fromJust $ elemIndex x universe
                j = fromJust $ elemIndex y universe
            in matrix !! i !! j

fromFunction :: (ResiduatedLattice l) => ((a, a) -> l) -> [a] -> FuzzyRelation a l
fromFunction = FuzzyRelation

membership :: (ResiduatedLattice l) => FuzzyRelation a l -> (a, a) -> l
membership (FuzzyRelation f _ ) = f

equality :: (ResiduatedLattice l) => (a -> l) -> a -> a -> l
equality f x y = f x <--> f y

alphaCut :: (ResiduatedLattice l) => l -> FuzzyRelation a l -> [(a, a)]
alphaCut alpha (FuzzyRelation f u) = [(x, y) | x <- u, y <- u, f (x, y) >= alpha]
