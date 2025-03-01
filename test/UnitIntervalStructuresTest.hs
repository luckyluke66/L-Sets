module UnitIntervalStructuresTest (
    godelTests,
    lukasiewiczTests,
    productTests,
) where

import Test.Tasty
import Test.Tasty.HUnit
import Lattices.UnitIntervalStructures.Godel
import Lattices.UnitIntervalStructures.Lukasiewicz
import Lattices.UnitIntervalStructures.Product
import Lattices.ResiduatedLattice
import Lattices.UnitInterval

assertApproxEqual :: (RealFrac a, Show a) => String -> a -> a -> Assertion
assertApproxEqual msg actual expected =
    assertBool (msg ++ " expected: " ++ show expected ++ " but got: " ++ show actual) (abs (expected - actual) < 1e-7)


godelTests :: TestTree
godelTests = testGroup "Godel Tests" [
    -- Test cases for BoundedLattice instance
    testCase "meet" $ assertApproxEqual "" (UIGodel 0.3 /\ UIGodel 0.7) (UIGodel 0.3),
    testCase "join" $ assertApproxEqual "" (UIGodel 0.3 \/ UIGodel 0.7) (UIGodel 0.7),

    -- Test cases for ResiduatedLattice instance
    testCase "tnorm 0.3 0.7" $ assertApproxEqual "" (tnorm (UIGodel 0.3) (UIGodel 0.7)) (UIGodel 0.3),
    testCase "tnorm 0.5 0.5" $ assertApproxEqual "" (tnorm (UIGodel 0.5) (UIGodel 0.5)) (UIGodel 0.5),
    testCase "tnorm 0.0 1.0" $ assertApproxEqual "" (tnorm (UIGodel 0.0) (UIGodel 1.0)) (UIGodel 0.0),
    testCase "tnorm 1.0 1.0" $ assertApproxEqual "" (tnorm (UIGodel 1.0) (UIGodel 1.0)) (UIGodel 1.0),
    testCase "tnorm 0.2 0.8" $ assertApproxEqual "" (tnorm (UIGodel 0.2) (UIGodel 0.8)) (UIGodel 0.2),
    testCase "residuum 0.3 0.7" $ assertApproxEqual "" (UIGodel 0.3 --> UIGodel 0.7) top,
    testCase "residuum 0.7 0.3" $ assertApproxEqual "" (UIGodel 0.7 --> UIGodel 0.3) (UIGodel 0.3),
    testCase "residuum 0.5 0.5" $ assertApproxEqual "" (UIGodel 0.5 --> UIGodel 0.5) top,
    testCase "residuum 0.2 0.8" $ assertApproxEqual "" (UIGodel 0.2 --> UIGodel 0.8) top,
    testCase "residuum 0.8 0.2" $ assertApproxEqual "" (UIGodel 0.8 --> UIGodel 0.2) (UIGodel 0.2)
    ]

lukasiewiczTests :: TestTree
lukasiewiczTests = testGroup "Lukasiewicz Tests" [
    -- Test cases for BoundedLattice instance
    testCase "meet" $ assertApproxEqual "" (UILukasiewicz 0.3 /\ UILukasiewicz 0.7) (UILukasiewicz 0.3),
    testCase "join" $ assertApproxEqual "" (UILukasiewicz 0.3 \/ UILukasiewicz 0.7) (UILukasiewicz 0.7),

    -- Test cases for ResiduatedLattice instance
    testCase "tnorm 0.3 0.7" $ assertApproxEqual "" (tnorm (UILukasiewicz 0.3) (UILukasiewicz 0.7)) bot,
    testCase "tnorm 0.5 0.5" $ assertApproxEqual "" (tnorm (UILukasiewicz 0.5) (UILukasiewicz 0.5)) bot,
    testCase "tnorm 0.0 1.0" $ assertApproxEqual "" (tnorm (UILukasiewicz 0.0) (UILukasiewicz 1.0)) bot,
    testCase "tnorm 1.0 1.0" $ assertApproxEqual "" (tnorm (UILukasiewicz 1.0) (UILukasiewicz 1.0)) top,
    testCase "tnorm 0.0 0.0" $ assertApproxEqual "" (tnorm (UILukasiewicz 0.0) (UILukasiewicz 0.0)) bot,
    testCase "tnorm 0.2 0.8" $ assertApproxEqual "" (tnorm (UILukasiewicz 0.2) (UILukasiewicz 0.8)) bot,
    testCase "residuum 0.3 0.7" $ assertApproxEqual "" (UILukasiewicz 0.3 --> UILukasiewicz 0.7) bot,
    testCase "residuum 0.7 0.3" $ assertApproxEqual "" (UILukasiewicz 0.7 --> UILukasiewicz 0.3) bot,
    testCase "residuum 0.5 0.5" $ assertApproxEqual "" (UILukasiewicz 0.5 --> UILukasiewicz 0.5) bot,
    testCase "residuum 0.2 0.8" $ assertApproxEqual "" (UILukasiewicz 0.2 --> UILukasiewicz 0.8) bot,
    testCase "residuum 0.8 0.2" $ assertApproxEqual "" (UILukasiewicz 0.8 --> UILukasiewicz 0.2) bot
    ]

productTests :: TestTree
productTests = testGroup "Product Tests" [
    -- Test cases for BoundedLattice instance
    testCase "meet" $ assertApproxEqual "" (UIProduct 0.2 /\ UIProduct 0.1) (UIProduct 0.1),
    testCase "join" $ assertApproxEqual "" (UIProduct 0.3 \/ UIProduct 0.5) (UIProduct 0.5),
    
    -- Test cases for ResiduatedLattice instance
    testCase "tnorm 0.3 0.7" $ assertApproxEqual "" (tnorm (UIProduct 0.3) (UIProduct 0.7)) (UIProduct 0.3 * 0.7),
    testCase "tnorm 0.5 0.5" $ assertApproxEqual "" (tnorm (UIProduct 0.5) (UIProduct 0.5)) (UIProduct 0.5 * 0.5),
    testCase "tnorm 0.0 0.0" $ assertApproxEqual "" (tnorm (UIProduct 0.0) (UIProduct 0.0)) (UIProduct 0.0),
    testCase "tnorm 0.0 1.0" $ assertApproxEqual "" (tnorm (UIProduct 0.0) (UIProduct 1.0)) (UIProduct 0.0),
    testCase "tnorm 1.0 1.0" $ assertApproxEqual "" (tnorm (UIProduct 1.0) (UIProduct 1.0)) (UIProduct 1.0),
    testCase "tnorm 0.2 0.8" $ assertApproxEqual "" (tnorm (UIProduct 0.2) (UIProduct 0.8)) (UIProduct 0.16),
    testCase "tnorm 0.7 0.7" $ assertApproxEqual "" (tnorm (UIProduct 0.7) (UIProduct 0.7)) (UIProduct 0.49),
    testCase "residuum 0.3 0.7" $ assertApproxEqual "" (UIProduct 0.3 --> UIProduct 0.7) (UIProduct top),
    testCase "residuum 0.7 0.3" $ assertApproxEqual "" (UIProduct 0.7 --> UIProduct 0.3) (UIProduct 0.3 / 0.7),
    testCase "residuum 0.5 0.5" $ assertApproxEqual "" (UIProduct 0.5 --> UIProduct 0.5) (UIProduct top),
    testCase "residuum 0.2 0.8" $ assertApproxEqual "" (UIProduct 0.2 --> UIProduct 0.8) (UIProduct top),
    testCase "residuum 0.8 0.2" $ assertApproxEqual "" (UIProduct 0.8 --> UIProduct 0.2) (UIProduct 0.2 / 0.8)
    ]