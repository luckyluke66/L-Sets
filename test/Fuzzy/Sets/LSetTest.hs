module Fuzzy.Sets.LSetTest (
    lsetTests
) where 

import Test.Tasty
import Test.Tasty.HUnit
import Fuzzy.Sets.LSet
import Lattices.UnitInterval
import Lattices.ResiduatedLattice
import Lattices.UnitIntervalStructures.Godel
import Fuzzy.Sets.MembershipFunctions

pairs = zip [1..10] (map mkGodelUnitInterval [0..1])


lsetTests :: TestTree
lsetTests = testGroup "LSet Tests" [
    testCase "fromPairs toPairs identity" $ assertEqual "" ((toPairs . fromPairs) pairs) pairs,
    testCase "fromPairs creates correct LSet" testFromPairs,
    testCase "fromFunction creates correct LSet" testFromFunction,
    testCase "toPairs converts LSet to correct list of pairs" testToPairs
    ]

testFromPairs :: Assertion
testFromPairs = do
    let pairs = [(1, 0.5), (2, 0.8)]
        lset = fromPairs pairs :: LSet Int UIGodel
    assertEqual "fromPairs membership function" (member lset 1) 0.5
    assertEqual "fromPairs membership function" (member lset 2) 0.8
    assertEqual "fromPairs universe" (universe lset) [1, 2]

testFromFunction :: Assertion
testFromFunction = do
    let f x = if x == 1 then 0.5 else 0.8
        u = [1, 2]
        lset = fromFunction f u :: LSet Int UIGodel
    assertEqual "fromFunction membership function" (member lset 1) 0.5
    assertEqual "fromFunction membership function" (member lset 2) 0.8
    assertEqual "fromFunction universe" (universe lset) [1, 2]

testToPairs :: Assertion
testToPairs = do
    let f x = if x == 1 then 0.5 else 0.8
        u = [1, 2]
        lset = fromFunction f u :: LSet Int UIGodel
        pairs = toPairs lset
    assertEqual "toPairs result" pairs [(1, 0.5), (2, 0.8)]