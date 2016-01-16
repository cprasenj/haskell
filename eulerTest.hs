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

    describe "largestPrimeFactor" $ do

        it "should give 29 for 13195" $ do
            largestPrimeFactor 13195 `shouldBe` 29

--        it "should give 6857 for 600851475143" $ do
--            largestPrimeFactor 600851475143 `shouldBe` 6857

    describe "largestPrimeFactor" $ do
        it "should reverse a given Integer 123 -> 321" $ do
            reverseInt 123 `shouldBe` 321

    describe "isPalindrome" $ do

        it "should give False for 123" $ do
            isPalindrome 123 `shouldBe` False

        it "should give True for 123" $ do
            isPalindrome 323 `shouldBe` True

    describe "largestPalindromeProduct" $ do

        it "should give 9009 for 99" $ do
            largestPalindromeProduct 99 `shouldBe` 9009

--        it "should give 906609 for 999" $ do
--            largestPalindromeProduct 999 `shouldBe` 906609

    describe "smallestNumberDivisibleByRange" $ do

        it "should give 2520 for 10 10" $ do
            smallestNumberDivisibleByRange 10 10 `shouldBe` 2520

        it "should give 2520 for 20 20" $ do
            smallestNumberDivisibleByRange 20 20 `shouldBe` 232792560

    describe "sumOfSquere" $ do

        it "shiuld give 385 for 10" $ do
        sumOfSquere 10 `shouldBe` 385

    describe "squereOfSum" $ do

        it "shiuld give 3025 for 10" $ do
        squereOfSum 10 `shouldBe` 3025

    describe "diffOfSumOfSquereAndSquereOfSum" $ do

--        it "should give 2640 for 10" $ do
--        diffOfSumOfSquereAndSquereOfSum 10 `shouldBe` 2640

        it "should give 25164150 for 100" $ do
        diffOfSumOfSquereAndSquereOfSum 100 `shouldBe` 25164150
