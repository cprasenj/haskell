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

    let grid = "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08" ++
               "49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00" ++
               "81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65" ++
               "52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91" ++
               "22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80" ++
               "24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50" ++
               "32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70" ++
               "67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21" ++
               "24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72" ++
               "21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95" ++
               "78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92" ++
               "16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57" ++
               "86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58" ++
               "19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40" ++
               "04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66" ++
               "88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69" ++
               "04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36" ++
               "20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16" ++
               "20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54" ++
               "01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"

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

        it "should give 104743 for 10001" $ do
            nThPrime 10001 `shouldBe` 104743

    describe "slice" $ do

        it "should give [1, 2] for [1, 1, 1, 2, 4, 5] 2 2" $ do
            slice [1, 1, 1, 2, 4, 5] 2 2 `shouldBe` [1, 2]

        it "should give [1, 2] for [1, 1, 0, 1, 2, 4, 5] 3 2" $ do
            slice [1, 1, 0, 1, 2, 4, 5] 3 2 `shouldBe` [1, 2]

    describe "multipleOfChunkOfAList" $ do

        it "should give 4 for [1,2,2,3,4,5,5,6] 1 2" $ do
            multipleOfChunkOfAList [1,2,2,3,4,5,5,6] 1 2 `shouldBe` 4

        it "should give 12 for [1,2,2,3,4,5,5,6] 1 3" $ do
            multipleOfChunkOfAList [1,2,2,3,4,5,5,6] 1 3 `shouldBe` 12

--     describe "productOfNumbers" $ do

--         it "should give 5832 for given string and 4" $ do
--             productOfNumbers (toInt bigNum) 4 0 `shouldBe` 5832

--        it "should give 23514624000 for given string and 13" $ do
--            productOfNumbers (toInt bigNum) 13 0 `shouldBe` 23514624000

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

--        it "should give 37550402023 for 2000000" $ do
--            sumOfPrimes 2000000 `shouldBe` 142913828922
--
--    describe "sumOfPrimes" $ do
--        it "should give blah for blah" $ do
--            sanitizeSeries (toInt "123456023") 4 [] `shouldBe` [[1, 2, 3, 4], [2, 3, 4, 5], [3, 4, 5, 6]]
