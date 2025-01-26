module Main where

import FuzzySets.FuzzySet (FuzzySet(..), LSet(..))
import LSet(LSet(..))
import FuzzyRelations.FuzzyRelation 
import FuzzyRelations.MembershipFunctions
import FuzzySets.MembershipFunctions
import FuzzySets.Cardinality
import Lattices.UnitInterval
import Lattices.UnitIntervalStructures.Godel
import Lattices.UnitIntervalStructures.Lukasiewicz

examples = [100..200]

small :: FuzzySet Double UIGodel
small = FuzzySet (sigmoid 1.1 (-130)) (take 100 examples)

medium :: FuzzySet Double UIGodel
medium = FuzzySet (gaussian 1 (-175) (-14.5)) (take 10 examples)

tall :: FuzzySet Double UIGodel
tall = FuzzySet (sigmoid 0.8 (-178.5)) (take 10 examples)

aprox5 :: FuzzySet Double UILukasiewicz
aprox5 = FuzzySet (triangular 5 0) [1..10] 

isClose :: FuzzyRelation Double UILukasiewicz
isClose = FuzzyRelation isCloseTo [(a, b)| a <- [1.. 10], b <- [1..3]]


main :: IO ()
main = do
    let a = member small 140
    print a
    print $ universe small

    let b = member isClose (1, 1)
    print b
    print $ universe isClose