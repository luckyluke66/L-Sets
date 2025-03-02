module Fuzzy.Sets.PropertiesTest (
    propertiesTests
) where

import Utils.Utils
import Test.Tasty
import Test.Tasty.HUnit
import Fuzzy.Sets.Properties
import Fuzzy.Sets.LSet
import Lattices.ResiduatedLattice
import Lattices.UnitIntervalStructures.Godel
import Lattices.UnitIntervalStructures.Lukasiewicz
import Lattices.UnitIntervalStructures.Product


propertiesTests :: TestTree
propertiesTests = testGroup "Properties Tests" [
    testCase "isEmpty function" testIsEmpty,
    testCase "isSingleton function" testIsSingleton,
    testCase "isCrisp function" testIsCrisp,
    testCase "isUniversal function" testIsUniversal,
    testCase "strictSubsethood function" testStrictSubsethood,
    testCase "strictEquality function" testStrictEquality,
    testCase "gradedSubsethood function godel (crisp only)" testGradedSubsethoodGodel,
    testCase "gradedSubsethood function godel" testGradedSubsethoodGodel1,
    testCase "gradedSubsethood function lukasiewicz" testGradedSubsethoodLukasiewicz,
    testCase "gradedSubsethood function product" testGradedSubsethoodProduct,
    testCase "gradedEquality function godel(crisp only)" testGradedEqualityGodel,
    testCase "gradedEquality function godel" testGradedEqualityGodel1,
    testCase "gradedEquality function lukasiewicz" testGradedEqualityLukasiewicz,
    testCase "gradedEquality function product" testGradedEqualityProduct
    ]

testIsEmpty :: Assertion
testIsEmpty = do
    let lset = fromPairs [(1, bot), (2, bot)] :: LSet Int UIGodel
    assertBool "isEmpty function" (isEmpty lset)

testIsSingleton :: Assertion
testIsSingleton = do
    let lset = fromPairs [(1, bot), (2, top)] :: LSet Int UIGodel
    assertBool "isSingleton function" (isSingleton lset)

testIsCrisp :: Assertion
testIsCrisp = do
    let lset = fromPairs [(1, bot), (2, top)] :: LSet Int UIGodel
    assertBool "isCrisp function" (isCrisp lset)

testIsUniversal :: Assertion
testIsUniversal = do
    let lset = fromPairs [(1, top), (2, top)] :: LSet Int UIGodel
    assertBool "isUniversal function" (isUniversal lset)

testStrictSubsethood :: Assertion
testStrictSubsethood = do
    let lset1 = fromPairs [(1, top), (2, bot)] :: LSet Int UIGodel
        lset2 = fromPairs [(1, top), (2, top), (3, 0.5)] :: LSet Int UIGodel
    assertBool "strictSubsethood function" (strictSubsethood lset1 lset2)

testStrictEquality :: Assertion
testStrictEquality = do
    let lset1 = fromPairs [(1, top), (2, top)] :: LSet Int UIGodel
        lset2 = fromPairs [(1, top), (2, top)] :: LSet Int UIGodel
    assertBool "strictEquality function" (strictEquality lset1 lset2)


testGradedSubsethoodGodel :: Assertion
testGradedSubsethoodGodel = do
    let lset1 = fromPairs [(1, bot), (2, top)] :: LSet Int UIGodel
        lset2 = fromPairs [(1, bot), (2, top)] :: LSet Int UIGodel
    assertApproxEqual "gradedSubsethood function" (gradedSubsethood lset1 lset2) top


testGradedSubsethoodGodel1 :: Assertion
testGradedSubsethoodGodel1 = do
    let lset1 = fromPairs [(1, 0.3), (2, 0.5)] :: LSet Int UIGodel
        lset2 = fromPairs [(1, 0.4), (2, 0.6)] :: LSet Int UIGodel
    assertApproxEqual "gradedSubsethood function with varied values" (gradedSubsethood lset1 lset2) top


testGradedSubsethoodLukasiewicz :: Assertion
testGradedSubsethoodLukasiewicz = do
    let lset1 = fromPairs [(1, 0.5), (2, 0.5)] :: LSet Int UILukasiewicz
        lset2 = fromPairs [(1, 0.4), (2, 0.6)] :: LSet Int UILukasiewicz
    assertApproxEqual "gradedSubsethood function with varied values (UILukasiewicz)" (gradedSubsethood lset1 lset2) (UILukasiewicz 0.9)


testGradedSubsethoodProduct :: Assertion
testGradedSubsethoodProduct = do
    let lset1 = fromPairs [(1, 0.3), (2, 0.5)] :: LSet Int UIProduct
        lset2 = fromPairs [(1, 0.2), (2, 0.6)] :: LSet Int UIProduct
    assertApproxEqual "gradedSubsethood function with varied values (UIProduct)" (gradedSubsethood lset1 lset2) (UIProduct $ 0.2 / 0.3)


testGradedEqualityGodel :: Assertion
testGradedEqualityGodel = do
    let lset1 = fromPairs [(1, top), (2, top)] :: LSet Int UIGodel
        lset2 = fromPairs [(1, top), (2, top)] :: LSet Int UIGodel
    assertApproxEqual "gradedEquality function" (gradedEquality lset1 lset2) top 


testGradedEqualityGodel1 :: Assertion
testGradedEqualityGodel1 = do
    let lset1 = fromPairs [(1, 0.3), (2, 0.5)] :: LSet Int UIGodel
        lset2 = fromPairs [(1, 0.4), (2, 0.4)] :: LSet Int UIGodel
    assertApproxEqual "gradedEquality function with varied values" (gradedEquality lset1 lset2) (UIGodel 0.3)


testGradedEqualityLukasiewicz :: Assertion
testGradedEqualityLukasiewicz = do
    let lset1 = fromPairs [(1, 0.2), (2, 0.7)] :: LSet Int UILukasiewicz
        lset2 = fromPairs [(1, 0.3), (2, 0.4)] :: LSet Int UILukasiewicz
    assertApproxEqual "gradedEquality function with varied values (UILukasiewicz)" (gradedEquality lset1 lset2) (UILukasiewicz 0.7) 


testGradedEqualityProduct :: Assertion
testGradedEqualityProduct = do
    let lset1 = fromPairs [(1, 1), (2, 0.6)] :: LSet Int UIProduct
        lset2 = fromPairs [(1, 0.9), (2, 0.5)] :: LSet Int UIProduct
    assertApproxEqual "gradedEquality function with varied values (UIProduct)" (gradedEquality lset1 lset2) (UIProduct 0.5/0.6)
