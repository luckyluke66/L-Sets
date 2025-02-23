module Main where

import FuzzySets.LSet
import FuzzySet
import FuzzyRelations.LRelation 
import FuzzyRelations.MembershipFunctions
import FuzzySets.MembershipFunctions
import FuzzySets.Cardinality
import Lattices.UnitInterval
import Lattices.UnitIntervalStructures.Godel
import Lattices.UnitIntervalStructures.Lukasiewicz

examples = [100..200]

small :: LSet Double UIGodel
small = LSet (sigmoid 1.1 (-130)) (take 100 examples)

medium :: LSet Double UIGodel
medium = LSet (gaussian 1 (-175) (-14.5)) (take 10 examples)

tall :: LSet Double UIGodel
tall = LSet (sigmoid 0.8 (-178.5)) (take 10 examples)

aprox5 :: LSet Double UILukasiewicz
aprox5 = LSet (triangular 5 0) [1..10] 

isClose :: LRelation Double UILukasiewicz
isClose = LRelation isCloseTo [(a, b)| a <- [1.. 10], b <- [1..3]]


main :: IO ()
main = do
    let a = member small 140
    print a
    print $ universe small

    let b = member isClose (1, 1)
    print b
    print $ universe isClose