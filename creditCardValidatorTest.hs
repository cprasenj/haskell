module CreditCardValidatorTest where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import CreditCardValidator

main :: IO ()
main = hspec $ do
  describe "toDigits" $ do

    it "should give [1,0] for 10" $ do
      toDigits 10 `shouldBe` [1,0]

    it "should give [1,2] for 12" $ do
      toDigits 12 `shouldBe` [1, 2]

    it "should give [1,2,3,4,5] for 12345" $ do
      toDigits 12345 `shouldBe` [1, 2, 3, 4, 5]

    it "should give [] for 0" $ do
      toDigits 0 `shouldBe` []

    it "should give [] for negative numbers" $ do
      toDigits (-17) `shouldBe` []

  describe "toDigitsRev" $ do

    it "should give reverese of toDigits" $ do
      toDigitsRev 1 `shouldBe` [1]

    it "should give [2,1] for 12" $ do
      toDigitsRev 12 `shouldBe` [2, 1]

    it "should give [5,4,3,2,1] for 12345" $ do
      toDigitsRev 12345 `shouldBe` [5,4,3,2,1]

    it "should give [] for 0" $ do
      toDigitsRev 0 `shouldBe` []

    it "should give [] for negative numbers" $ do
      toDigitsRev (-17) `shouldBe` []

  describe "doubleEveryOther" $ do

    it "should give [2] for [1]" $ do
      doubleEveryOther [1] `shouldBe` [2]

    it "should give [1,1,1] for [2,1,2]" $ do
      doubleEveryOther [1,1,1] `shouldBe` [2,1,2]

    it "should give [1,1,1,1,1,1] for [2,1,2,1,2,1]" $ do
      doubleEveryOther [1,1,1] `shouldBe` [2,1,2]

  describe "sumDigitsOfANumber" $ do

    it "should give 2 for 2" $ do
      sumDigitsOfANumber 2 `shouldBe` 2

    it "should give 2 for 11" $ do
      sumDigitsOfANumber 11 `shouldBe` 2

  describe "sumDigits" $ do

    it "should give 0 for []" $ do
      sumDigits [] `shouldBe` 0

    it "should give 1 for [1]" $ do
      sumDigits [1] `shouldBe` 1

    it "should give 4 for [4]" $ do
      sumDigits [4] `shouldBe` 4

    it "should give 4 for [13]" $ do
      sumDigits [13] `shouldBe` 4

    it "should give 4 for [1,1,1,1]" $ do
      sumDigits [1,1,1,1] `shouldBe` 4

    it "should give 14 for [1,12,18,1]" $ do
      sumDigits [1,12,18,1] `shouldBe` 14

  describe "validate" $ do

    it "should give True for  4012888888881881" $ do
      validate 4012888888881881 `shouldBe` True

    it "should give false for  4012888888881882" $ do
      validate 4012888888881882 `shouldBe` False
