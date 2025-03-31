module Fuzzy.Relations.PropertiesTest (
    relPropertiesTests
) where

import Test.Tasty
import Test.Tasty.HUnit
import Fuzzy.Relations.LRelation
import Fuzzy.Relations.Properties
import Lattices.ResiduatedLattice
import Utils.Utils (assertApproxEqual)
import Lattices.UnitIntervalStructures.Godel

relPropertiesTests :: TestTree
relPropertiesTests = testGroup "Properties Tests" [
    testCase "ref function (top case)" testRefTop,
    testCase "ref function (bot case)" testRefBot,
    testCase "sym function (top case)" testSymTop,
    testCase "sym function (bot case)" testSymBot,
    testCase "tra function (top case)" testTraTop,
    testCase "irref function (top case)" testIrrefTop,
    testCase "irref function (bot case)" testIrrefBot,
    testCase "asym function (top case)" testAsymTop,
    testCase "asym function (bot case)" testAsymBot
    ]

u = [(x, y) | x <- [1, 2, 3], y <- [1, 2, 3]]

-- Reflexivity Tests
testRefTop :: Assertion
testRefTop = do
    let rel = LRelation (\(x, y) -> if x == y then top else bot) u :: LRelation Int UIGodel
    assertApproxEqual "ref function (top case)" (ref rel) top


testRefBot :: Assertion
testRefBot = do
    let rel = LRelation (\(x, y) -> bot) u :: LRelation Int UIGodel
    assertApproxEqual "ref function (bot case)" (ref rel) bot


-- Symmetry Tests
testSymTop :: Assertion
testSymTop = do
    let rel = LRelation (\(x, y) -> if x == y || (x, y) == (1, 2) || (x, y) == (2, 1) then top else bot) u :: LRelation Int UIGodel
    assertApproxEqual "sym function (top case)" (sym rel) top


testSymBot :: Assertion
testSymBot = do
    let rel = LRelation (\(x, y) -> if (x, y) == (1, 2) then top else bot) u :: LRelation Int UIGodel
    assertApproxEqual "sym function (bot case)" (sym rel) bot


-- Transitivity Tests
testTraTop :: Assertion
testTraTop = do
    let rel = LRelation (\(x, y) -> if x <= y then top else bot) u :: LRelation Int UIGodel
    assertApproxEqual "tra function (top case)" (tra rel) top


-- Irreflexivity Tests
testIrrefTop :: Assertion
testIrrefTop = do
    let rel = LRelation (\(x, y) -> if x == y then bot else top) u :: LRelation Int UIGodel
    assertApproxEqual "irref function (top case)" (irref rel) top


testIrrefBot :: Assertion
testIrrefBot = do
    let rel = LRelation (\(x, y) -> if x == y then top else bot) u :: LRelation Int UIGodel
    assertApproxEqual "irref function (bot case)" (irref rel) bot


-- Asymmetry Tests
testAsymTop :: Assertion
testAsymTop = do
    let rel = LRelation (\(x, y) -> if x < y then top else bot) u :: LRelation Int UIGodel
    assertApproxEqual "asym function (top case)" (asym rel) top


testAsymBot :: Assertion
testAsymBot = do
    let rel = LRelation (\(x, y) -> if x <= y then top else bot) u :: LRelation Int UIGodel
    assertApproxEqual "asym function (bot case)" (asym rel) bot