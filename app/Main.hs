module Main where

import FuzzySet(FuzzySet(FuzzySet), membership, crop)
import MembershipFunctions(linear, triangular, gaussian, sigmoid, rectangular, trapezoid, constant)


main :: IO ()
main = do
    -- fuzzy set page 77 in Fuzzy Relational Sructures 
    print "aprox5"
    let aprox5 = FuzzySet (triangular 5 0)
    let examples = [4, 4.2, 4.5, 4.8, 5, 5.2, 5.5, 6]
    mapM_ (print . membership aprox5) examples

    -- fuzzy set from page 78 1)
    print "height"
    let small = FuzzySet (sigmoid 1.1 (-130) 0.9)
    let medium = FuzzySet (gaussian 1 (-175) (-14.5))
    let tall = FuzzySet (sigmoid 0.8 (-178.5) 1.2)
    let examples = [100, 120..]
    print "small"
    mapM_ (print . membership small) $ take 10 examples
    print "medium"
    mapM_ (print . membership medium) $ take 10 examples
    print "tall"
    mapM_(print . membership tall) $ take 10 examples


