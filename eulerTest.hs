module EulerTest where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Euler

main :: IO ()
main = hspec $ do
    describe "multipleOf3Or5" $ do

        it "should give True for 3" $ do
            multipleOf3Or5 3 `shouldBe` True

        it "should give True for 5" $ do
            multipleOf3Or5 5 `shouldBe` True

        it "should give True for 15" $ do
            multipleOf3Or5 15 `shouldBe` True

        it "should give True for 15" $ do
            multipleOf3Or5 15 `shouldBe` True

    describe "multipleOf17Or19" $ do
        it "should give True for 17" $ do
            multipleOf17Or19 17 `shouldBe` True

    describe "sumOfMultiPlesInAGivenRangeUnderACondition" $ do

        it "should give 0 for first number greater than second" $ do
            sumOfMultiPlesInAGivenRangeUnderACondition 14 12  multipleOf3Or5 `shouldBe` 0

        it "should give 23 for 0 10 multipleOf3Or5" $ do
            sumOfMultiPlesInAGivenRangeUnderACondition 0 9  multipleOf3Or5 `shouldBe` 23

        it "should give 233168 for 0 999 multipleOf3Or5" $ do
            sumOfMultiPlesInAGivenRangeUnderACondition 0 999  multipleOf3Or5 `shouldBe` 233168

        it "should give 36 for 0 19 multipleOf3Or5" $ do
            sumOfMultiPlesInAGivenRangeUnderACondition 0 19  multipleOf17Or19 `shouldBe` 36

    describe "fibonachi" $ do

        it "should give empty list for wrong argument" $ do
            fibonachi 3 2 1 `shouldBe` []

        it "should give empty list for wrong argument" $ do
            fibonachi 3 2 (-1) `shouldBe` []

        it "should give [0] for 0 1 1" $ do
            fibonachi 0 1 2 `shouldBe` [0, 1, 1]

        it "should give [0,1] for 0 1 2" $ do
            fibonachi 0 1 2 `shouldBe` [0, 1, 1]

        it "should give [0, 1, 1, 2, 3, 5, 8] for 0 1 10" $ do
            fibonachi 0 1 10 `shouldBe` [0, 1, 1, 2, 3, 5, 8]

    describe "isEven" $ do

        it "should give True for 4" $ do
            isEven 4 `shouldBe` True

        it "should give False for 4" $ do
            isEven 5 `shouldBe` False

    describe "createSumOfFilteredList" $ do

        it "should give 2 for 1 2 2 fibonachi isEven" $ do
            createSumOfFilteredList 1 2 3 fibonachi isEven `shouldBe` 2

        it "should give 4613732 for 1 2 2 fibonachi isEven" $ do
            createSumOfFilteredList 1 2 3999999 fibonachi isEven `shouldBe` 4613732