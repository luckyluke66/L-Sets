module Utils.Utils (
    assertApproxEqual,
) where

import Test.Tasty.HUnit

assertApproxEqual :: (RealFrac a, Show a) => String -> a -> a -> Assertion
assertApproxEqual msg actual expected =
    assertBool (msg ++ " expected: " ++ show expected ++ " but got: " ++ show actual) (abs (expected - actual) < 1e-7)
