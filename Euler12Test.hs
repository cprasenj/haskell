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

  it "gives [1, 3] for findDivisors 3" $ do
    areEqual (findDivisors 3) [1, 3] `shouldBe` True
  it "gives [1, 3, 9] for findDivisors 9" $ do
    areEqual (findDivisors 9) [1, 3, 9] `shouldBe` True
  it "gives [1, 2, 3, 4, 6, 12] for findDivisors 12" $ do
    areEqual (findDivisors 12) [1, 2, 3, 4, 6, 12] `shouldBe` True

describe "triangularNumberWithDivisor" $ do
  it "gives 28 for 5" $ do
    triangularNumberWithDivisor 5 `shouldBe` 28

  it "gives 28 for 500" $ do
    triangularNumberWithDivisor 500 `shouldBe` 28
