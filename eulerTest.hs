module EulerTest where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Euler

main :: IO ()
main = hspec $ do
    let bigNum =  "73167176531330624919225119674426574742355349194934" ++
                  "96983520312774506326239578318016984801869478851843" ++
                  "85861560789112949495459501737958331952853208805511" ++
                  "12540698747158523863050715693290963295227443043557" ++
                  "66896648950445244523161731856403098711121722383113" ++
                  "62229893423380308135336276614282806444486645238749" ++
                  "30358907296290491560440772390713810515859307960866" ++
                  "70172427121883998797908792274921901699720888093776" ++
                  "65727333001053367881220235421809751254540594752243" ++
                  "52584907711670556013604839586446706324415722155397" ++
                  "53697817977846174064955149290862569321978468622482" ++
                  "83972241375657056057490261407972968652414535100474" ++
                  "82166370484403199890008895243450658541227588666881" ++
                  "16427171479924442928230863465674813919123162824586" ++
                  "17866458359124566529476545682848912883142607690042" ++
                  "24219022671055626321111109370544217506941658960408" ++
                  "07198403850962455444362981230987879927244284909188" ++
                  "84580156166097919133875499200524063689912560717606" ++
                  "05886116467109405077541002256983155200055935729725" ++
                  "71636269561882670428252483600823257530420752963450"

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

        it "should give 2640 for 10" $ do
            diffOfSumOfSquereAndSquereOfSum 10 `shouldBe` 2640

        it "should give 25164150 for 100" $ do
            diffOfSumOfSquereAndSquereOfSum 100 `shouldBe` 25164150

    describe "nThPrime" $ do

        it "should give 13 for 6" $ do
            nThPrime 6 `shouldBe` 13

--        it "should give 104743 for 10001" $ do
--            nThPrime 10001 `shouldBe` 104743

    describe "productOfNNumbers" $ do

        it "should give 5832 for given string and 4" $ do
            productOfNNumbers 4 0 (toInt bigNum) `shouldBe` 5832

        it "should give 23514624000 for given string and 13" $ do
            productOfNNumbers 13 0 (toInt bigNum) `shouldBe` 23514624000

    describe "pythagoreanTripletFinder" $ do

        it "should give [[3,4,5]] for 12" $ do
            pythagoreanTripletFinder 12 `shouldBe` [[3,4,5]]

        it "should give [[375,200,425]] for 1000" $ do
            pythagoreanTripletFinder 1000 `shouldBe` [[375,200,425]]

    describe "pythagoreanTripletProduct" $ do
        it "should give 31875000 for 1000" $ do
            pythagoreanTripletProduct 1000 `shouldBe` 31875000

    describe "sumOfPrimes" $ do
        it "should give 17 for 10" $ do
            sumOfPrimes 10 `shouldBe` 17

--        it "should give 37550402023 for 1000000" $ do
--            sumOfPrimes 1000000 `shouldBe` 17

        it "should give 37550402023 for 2000000" $ do
            sumOfPrimes 2000000 `shouldBe` 17