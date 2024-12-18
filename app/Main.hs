module Main where

import FuzzySets.FuzzySet ( universe, membership, FuzzySet(..) )
import FuzzySets.MembershipFunctions
import FuzzySets.Cardinality
import Lattices.UnitInterval
import Lattices.UnitIntervalStructures.Godel
import Lattices.UnitIntervalStructures.Lukasiewicz

examples = [100..200]

small :: FuzzySet Double UIGodel
small = FuzzySet (sigmoid 1.1 (-130)) (take 10 examples)

medium :: FuzzySet Double UIGodel
medium = FuzzySet (gaussian 1 (-175) (-14.5)) (take 10 examples)

tall :: FuzzySet Double UIGodel
tall = FuzzySet (sigmoid 0.8 (-178.5)) (take 10 examples)

aprox5 :: FuzzySet Double UILukasiewicz
aprox5 = FuzzySet (triangular 5 0) [1..10] 

main :: IO ()
main = do
    --fuzzy set page 77 in Fuzzy Relational Sructures 
    print "aprox5"
    mapM_ (print . membership aprox5) $ universe aprox5

    --fuzzy sets from page 78 1)
    print "height"
    print "--------------------"
    print "small"
    mapM_ (print . membership small) $ universe small
    --print "medium"
    --mapM_ (print . membership medium) $ take 10 examples
    --print "tall"
    --mapM_(print . membership tall) $ take 10 examples
    print "-------cardinality-------"

    print $ normalizedSigmaCount small
    print $ sigmaCount tall