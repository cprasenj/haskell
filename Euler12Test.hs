module Euler12Test where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Euler12
import Data.List

areEqual list1 list2 = sort list1 == sort list1

main :: IO ()
main = hspec $ do

    describe "findDivisors" $ do

        it "gives [1, 3] for findDivisors 3 1 []" $ do
            areEqual (findDivisors 3 1 []) [1, 3] `shouldBe` True

    describe "triangleNumbersWithDivisors" $ do

        it "gives [1, 3] for triangleNumbersWithDivisors 3 1 []" $ do
            triangleNumbersWithDivisors 1 1 6 `shouldBe` 28
--             triangleNumbersWithDivisors 1 6 `shouldBe` 28
--             triangleNumbersWithDivisors 1 20 `shouldBe` 209628
--             triangleNumbersWithDivisors 1 20 `shouldBe` 209628
            triangleNumbersWithDivisors 1 1 500 `shouldBe` 73920
