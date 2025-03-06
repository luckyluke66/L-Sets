module Main (main) where
import Test.Tasty
import Lattices.UnitIntervalStructures.Godel
import Lattices.ResiduatedLattice
import Lattices.UnitInterval
import UnitIntervalStructuresTest
import Fuzzy.Sets.LSetTest 
import Fuzzy.Sets.MembershipFunctionsTest
import Fuzzy.Sets.PropertiesTest 

-- Run all tests
main :: IO ()
main = defaultMain $ testGroup "All Tests" [
    godelTests,
    lukasiewiczTests,
    productTests,
    lsetTests,
    membershipFunctionsTests,
    propertiesTests
    ]