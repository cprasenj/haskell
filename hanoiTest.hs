module CreditCardValidatorTest where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Hanoi

main :: IO ()
main = hspec $ do

  describe "hanoi" $ do

    it "should give  [(a,c), (a,b), (c,b)] for hanoi 2 a b c" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]
