module PracticeTest where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Practice

main :: IO ()
main = hspec $ do
  describe "mylast" $ do

    it "returns the last element of a list for an integer list" $ do
      mylast [23, 1, 2, 3, 4] `shouldBe` (4 :: Int)

    it "returns the last element of a list for an character list" $ do
      mylast ['a', 'b', 'c', 'd', 'e'] `shouldBe` ('e' :: Char)

    it "returns the last element of a list for an string list" $ do
      mylast ["apple", "ball", "cat", "dog", "elephant"] `shouldBe` ("elephant" :: [Char])

    it "returns the last element of a list for an list of lists" $ do
      mylast [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12], [13, 14, 15]] `shouldBe` ([13, 14, 15] :: [Integer])

    it "gives error for empty list" $ do
      mylast [] `shouldThrow` anyException

  describe "mySecondLast" $ do

    it "returns the second last element of a list for an integer list" $ do
      mySecondLast [23, 1, 2, 3, 4] `shouldBe` (3 :: Int)

    it "returns the second last element of a list for an character list" $ do
      mySecondLast ['a', 'b', 'c', 'd', 'e'] `shouldBe` ('d' :: Char)

    it "returns the second last element of a list for an string list" $ do
      mySecondLast ["apple", "ball", "cat", "dog", "elephant"] `shouldBe` ("dog" :: [Char])

    it "returns the last element of a list for an list of lists" $ do
      mySecondLast[[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12], [13, 14, 15]] `shouldBe` ([10, 11, 12] :: [Integer])

    it "gives no element found error for empty list" $ do
      mySecondLast [] `shouldThrow` anyException

    it "gives error if there is only one element in the list" $ do
      evaluate(mySecondLast [1]) `shouldThrow` anyException

  describe "findKthElement" $ do

    it "gives error if asked for 0th element" $ do
      evaluate(findKthElement [1, 2, 3, 4, 5] 0) `shouldThrow` anyException

    it "gives error if asked for negative indexed element" $ do
      evaluate(findKthElement [1, 2, 3, 4, 5] (-1)) `shouldThrow` anyException

    it "gives error if asked for out of range indexed element" $ do
      evaluate(findKthElement [1, 2, 3, 4, 5] (10)) `shouldThrow` anyException

    it "returns the second element of a list for an integer list if asked" $ do
      findKthElement [23, 1, 2, 3, 4] 2 `shouldBe` (1 :: Int)

    it "returns the second element of a list for an character list if asked" $ do
      findKthElement ['a', 'b', 'c', 'd', 'e'] 2 `shouldBe` ('b' :: Char)

    it "returns the second element of a list for an string list if asked" $ do
      findKthElement ["apple", "ball", "cat", "dog", "elephant"] 2 `shouldBe` ("ball" :: [Char])

    it "returns the second element of a list for an list of lists if asked" $ do
      findKthElement [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12], [13, 14, 15]] 2 `shouldBe` ([4, 5, 6] :: [Integer])

  describe "findELementCount" $ do

    it "should return the count of element in an integer list" $ do
      findELementCount [1, 2, 3, 4, 5] `shouldBe` (5 :: Integer)

    it "should return the count of element in a String" $ do
      findELementCount "Prasenjit Chakraborty" `shouldBe` (21 :: Integer)

    it "should return 0 for empty string" $ do
      findELementCount "" `shouldBe` (0 :: Integer)

    it "should give 5 for [apple, ball, cat, dog, elephant]" $ do
      findELementCount ["apple", "ball", "cat", "dog", "elephant"] `shouldBe` (5 :: Integer)

    it "should give 5 for [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12], [13, 14, 15]]" $ do
      findELementCount [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12], [13, 14, 15]] `shouldBe` (5 :: Integer)

  describe "reverseList" $ do

    -- need to give it a look
    -- it "should give empty list for empty list" $ do
      -- reverseList ([]::[Integer]) `shouldBe` ([]::[Integer])
    it "should give reverse list for a given integer list" $ do
      reverseList [1, 2, 3, 4, 5] `shouldBe` ([5, 4, 3, 2, 1] :: [Integer])

    it "should give reverse list for a given String" $ do
      reverseList "prasenjit" `shouldBe` ("tijnesarp" :: [Char])

  describe "isEqualList" $ do

    it "should give true for two similer strings" $ do
      isEqualList "prasenjit" "prasenjit" `shouldBe` (True :: Bool)

    -- it "should give true for two empty list" $ do
      -- isEqualList []::[Integer] []::[Integer] `shouldBe` (True::Bool)

    it "should give true for two similer strings" $ do
      isEqualList [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]  `shouldBe` (True :: Bool)

    it "should give true for two similer strings" $ do
      isEqualList [1, 2, 3, 4, 5] [1, 2, 3, 4, 5]  `shouldBe` (True :: Bool)

    it "should give false for different lists" $ do
      isEqualList [1, 2, 3, 4, 5] [1, 2, 3, 4, 5, 6]  `shouldBe` (False :: Bool)

  describe "isPalindrome" $ do

    it "should return true for a" $ do
      isPalindrome "a" `shouldBe` (True :: Bool)

    it "should return true for madam" $ do
      isPalindrome "madam" `shouldBe` (True :: Bool)

    it "should return true for [1, 2, 3, 2, 1]" $ do
      isPalindrome [1, 2, 3, 2, 1] `shouldBe` (True :: Bool)

  describe "flatten" $ do
    it "should give [1,2,3,4,5] for List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])" $ do
      flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldBe` [1, 2, 3, 4, 5]

  describe "compress" $ do

    it "should give abcd for aaabbbcccddd" $ do
      compress "aaabbbcccddd" `shouldBe` "abcd"

    it "should give [1, 2, 3, 4]for [1, 2, 3, 4]" $ do
      compress [1, 2, 3, 4] `shouldBe` ([1, 2, 3, 4] :: [Integer])
      compress [1,2,1] `shouldMatchList` [1,2]

  describe "createPack" $ do

    it "should create a pack of similer kind of element" $ do
      createPack [1,2,1] 1 `shouldBe` [1,1]

  describe "pack" $ do

    it "should create a pack of similer kind of element" $ do
      pack [1,2,1] `shouldMatchList` [[1,1], [2]]

    it "should create a pack of similer kind of element" $ do
      pack [1,2,1,3] `shouldMatchList` [[1,1], [2], [3]]

    it "should create a pack of similer kind of element" $ do
      pack [1,2,1,3,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0] `shouldMatchList` [[1,1,1,1], [2,2,2], [3,3,3], [4,4], [5,5], [6,6], [7,7], [8,8], [9,9], [0,0]]

    it "should create a pack of similer kind of element" $ do
      pack "aaaabccaadeeee" `shouldMatchList` ["b", "cc", "aaaaaa", "d", "eeee"]

  describe "countList" $ do

    it "should give the count and the head element of the list" $ do
      countList [1] `shouldBe` (1, 1)

    it "should give the count and the head element of the list" $ do
      countList "aaaaaaaaaa" `shouldBe` (10, 'a')

  describe "encode" $ do

    it "should give [(1, 'a')] for [a]" $ do
      encode "a" `shouldMatchList` [(1, 'a')]

    it "should give [(1, 'a'), (1, 'b')] for [a, b]" $ do
      encode ['a', 'b'] `shouldMatchList` [(1, 'a'), (1, 'b')]

    it "should give [1,2,1,3,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0] for [(4,1), (3,2), (3,3), (2,4), (2,5), (2,6), (2,7), (2,8), (2,9), (2,0)]" $ do
      encode [1,2,1,3,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0] `shouldMatchList` [(4,1), (3,2), (3,3), (2,4), (2,5), (2,6), (2,7), (2,8), (2,9), (2,0)]
