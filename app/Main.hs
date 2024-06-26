module Main where

import FuzzySet
import MembershipFunctions
import Lattices.UnitInterval
import Lattices.UnitIntervalStructures.Godel
import Lattices.UnitIntervalStructures.Lukasiewicz

small :: FuzzySet Double UIGodel
small = FuzzySet (sigmoid 1.1 (-130) 0.9) 

medium :: FuzzySet Double UIGodel
medium = FuzzySet (gaussian 1 (-175) (-14.5))

tall :: FuzzySet Double UIGodel
tall = FuzzySet (sigmoid 0.8 (-178.5) 1.2)

aprox5 :: FuzzySet Double UILukasiewicz
aprox5 = FuzzySet (triangular 5 0)

main :: IO ()
main = do
    -- fuzzy set page 77 in Fuzzy Relational Sructures 
    print "aprox5"
    
    let examples = [4, 4.2, 4.5, 4.8, 5, 5.2, 5.5, 6]
    mapM_ (print . membership aprox5) examples

    -- fuzzy sets from page 78 1)
    print "height"
    let examples = [100, 120..]
    print "small"
    mapM_ (print . membership small) $ take 10 examples
    print "medium"
    mapM_ (print . membership medium) $ take 10 examples
    print "tall"
    mapM_(print . membership tall) $ take 10 examples