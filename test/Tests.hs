module Main (main) where
import Test.Tasty
import Lattices.UnitIntervalStructures.Godel
import Lattices.ResiduatedLattice
import Lattices.UnitInterval
import UnitIntervalStructuresTest

-- Run all tests
main :: IO ()
main = defaultMain $ testGroup "All Tests" [
    godelTests,
    lukasiewiczTests,
    productTests
    ]