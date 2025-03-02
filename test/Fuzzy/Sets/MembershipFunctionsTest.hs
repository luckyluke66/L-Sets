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
    assertEqual "constant function" (f 1.0) (mkLattice 0.5)

testLinear :: Assertion
testLinear = do
    let f = linear 2.0 1.0 :: Double -> UIGodel
    assertEqual "linear function" (f 1.0) (mkLattice 3.0)

testSigmoid :: Assertion
testSigmoid = do
    let f = sigmoid 1.0 0.0 :: Double -> UIGodel
    assertEqual "sigmoid function" (f 0.0) (mkLattice 0.5)
    assertEqual "sigmoid function" (f 1.0) (mkLattice (1 / (1 + exp (-1.0))))

testTriangular :: Assertion
testTriangular = do
    let f = triangular 0.0 1.0 :: Double -> UILukasiewicz
    assertEqual "triangular function" (f 0.0) (mkLattice 1.0)
    assertEqual "triangular function" (f 1.0) (mkLattice 1.0)
    assertEqual "triangular function" (f 0.5) (mkLattice 1.5)

testRectangular :: Assertion
testRectangular = do
    let f = rectangular 0.0 1.0 0.5 :: Double -> UILukasiewicz
    assertEqual "rectangular function" (f 0.5) (mkLattice 0.5)
    assertEqual "rectangular function" (f 1.5) bot

testTrapezoid :: Assertion
testTrapezoid = do
    let f = trapezoid 0.0 1.0 2.0 :: Double -> UILukasiewicz
    assertEqual "trapezoid function" (f 0.5) (mkLattice 0.5)
    assertEqual "trapezoid function" (f 1.5) (mkLattice 0.5)
    assertEqual "trapezoid function" (f 2.5) bot

testGaussian :: Assertion
testGaussian = do
    let f = gaussian 1.0 0.0 1.0 :: Double -> UIProduct
    assertEqual "gaussian function" (f 0.0) (mkLattice 1.0)
    assertEqual "gaussian function" (f 1.0) (mkLattice (exp (-0.5)))

testExponential :: Assertion
testExponential = do
    let f = exponential :: Double -> UIProduct
    assertEqual "exponential function" (f 1.0) (mkLattice (exp 1))
    assertEqual "exponential function" (f 0.0) (mkLattice 1.0)