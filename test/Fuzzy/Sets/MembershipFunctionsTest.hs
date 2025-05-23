module Fuzzy.Sets.MembershipFunctionsTest (
    membershipFunctionsTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Fuzzy.Sets.MembershipFunctions
import Lattices.UnitInterval
import Lattices.ResiduatedLattice
import Lattices.UnitIntervalStructures.Godel (UIGodel(UIGodel))
import Lattices.UnitIntervalStructures.Lukasiewicz (UILukasiewicz(UILukasiewicz))
import Lattices.UnitIntervalStructures.Product (UIProduct(UIProduct))

membershipFunctionsTests :: TestTree
membershipFunctionsTests = testGroup "Membership Functions Tests" [
    testCase "constant function" testConstant,
    testCase "linear function" testLinear,
    testCase "sigmoid function" testSigmoid,
    testCase "triangular function" testTriangular,
    testCase "rectangular function" testRectangular,
    testCase "trapezoid function" testTrapezoid,
    testCase "gaussian function" testGaussian,
    testCase "exponential function" testExponential
    ]

testConstant :: Assertion
testConstant = do
    let f = constant 0.5 :: Double -> UIGodel
    assertEqual "constant 1.0" (mkLattice 0.5) (f 1.0)
    assertEqual "constant 0.5" (mkLattice 0.5) (f 0.3)

testLinear :: Assertion
testLinear = do
    let f = linear 2.0 1.0 :: Double -> UIGodel
    assertEqual "linear 1.0" (mkLattice 3.0) (f 1.0)
    assertEqual "linear 6.0" (mkLattice 13.0) (f 6.0)
    assertEqual "linear 0.3" (mkLattice 7.0) (f 3.0)

testSigmoid :: Assertion
testSigmoid = do
    let f = sigmoid 1.0 0.0 :: Double -> UIGodel
    assertEqual "sigmoid 0.0" (mkLattice 0.5) (f 0.0)
    assertEqual "sigmoid 1.0" (mkLattice (1 / (1 + exp (-1.0)))) (f 1.0)

testTriangular :: Assertion
testTriangular = do
    let f = triangular 0.0 0.5 1.0 :: Double -> UILukasiewicz
    assertEqual "triangular function" (mkLattice 0.0) (f 0.0)
    assertEqual "triangular function" (mkLattice 1.0) (f 0.5)
    assertEqual "triangular function" (mkLattice 0.6) (f 0.3)

testRectangular :: Assertion
testRectangular = do
    let f = rectangular 0.0 1.0 0.5 :: Double -> UILukasiewicz
    assertEqual "rectangular function" (mkLattice 0.5) (f 0.5)
    assertEqual "rectangular function" bot (f 1.5)

testTrapezoid :: Assertion
testTrapezoid = do
    let f = trapezoidal 0.0 0.5 1.5 2.0 :: Double -> UILukasiewicz
    assertEqual "trapezoid function at 0.0" (mkLattice 0.0) (f 0.0)
    assertEqual "trapezoid function at 0.5" (mkLattice 1.0) (f 0.5)
    assertEqual "trapezoid function at 1.0" (mkLattice 1.0) (f 1.0)
    assertEqual "trapezoid function at 1.5" (mkLattice 1.0) (f 1.5)
    assertEqual "trapezoid function at 2.0" (mkLattice 0.0) (f 2.0)
    assertEqual "trapezoid function at 2.5" (mkLattice 0.0) (f 2.5)

testGaussian :: Assertion
testGaussian = do
    let f = gaussian 1.0 0.0 1.0 :: Double -> UIProduct
    assertEqual "gaussian function"  (mkLattice 1.0) (f 0.0)
    assertEqual "gaussian function"  (mkLattice (exp (-0.5))) (f 1.0)

testExponential :: Assertion
testExponential = do
    let f = exponential :: Double -> UIProduct
    assertEqual "exponential function" (mkLattice (exp 1)) (f 1.0) 
    assertEqual "exponential function" (mkLattice 1.0) (f 0.0)