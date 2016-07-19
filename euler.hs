module Euler where
import Data.List
import Data.Char (digitToInt)

multipleOf3Or5 :: Integer -> Bool
multipleOf3Or5 a = or $ [(== 0).(mod a)] <*> [3 , 5]

multipleOf17Or19 :: Integer -> Bool
multipleOf17Or19 a =  or $ [(== 0).(mod a)] <*> [17 , 19]

sumOfMultiPlesInAGivenRangeUnderACondition :: Integer -> Integer -> (Integer -> Bool) -> Integer
sumOfMultiPlesInAGivenRangeUnderACondition first second predicate
    |first > second = 0
    |otherwise = foldl1 (+) (filter predicate [first..second])

fibonachi :: Integer -> Integer -> Integer ->[Integer]
fibonachi first second limit
    |or $ [first > second, limit <= first] = []
    |otherwise = first : fibonachi second (first + second) limit

isEven :: Integer -> Bool
isEven number = ((==) 0).(mod number) $ 2

createSumOfFilteredList :: Integer -> Integer -> Integer -> (Integer -> Integer-> Integer -> [Integer]) -> (Integer -> Bool) -> Integer
createSumOfFilteredList start end limit listCreator predicate  = foldl1 (+) (filter predicate (listCreator start end limit))

sieve :: (Integral a) => [a] -> [a]
sieve (n:ns) = n : sieve ns'
  where ns' = filter ((/= 0).flip rem n) (n:ns)

isDivisible :: (Integral a) => a -> a -> Bool
isDivisible n1 n2 = (== 0).(mod n1) $ n2


largestPrimeFactor :: Integer -> Integer
largestPrimeFactor number = head.reverse.(filter predicate) $ (takeWhile (< (round.sqrt.(/2).fromIntegral $ number)) (sieve [2..]))
                            where predicate = isDivisible number

reverseInt :: Integer -> Integer
reverseInt x = (*) (signum x).read.reverse.show.abs  $ x

isPalindrome :: Integer -> Bool
isPalindrome number = (== number).reverseInt $ number

palindromeFinder :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
palindromeFinder first second limit palindromeList
    |and $ [(== 1)] <*> [first, second] = palindromeList
    |and $ [second == 1, isPalindrome potentialPalindrome] = palindromeFinder (first-1) limit limit (potentialPalindrome : palindromeList)
    |second == 1 = palindromeFinder (first-1) limit limit palindromeList
    |isPalindrome potentialPalindrome = palindromeFinder first (second-1) limit (palindromeList ++ [potentialPalindrome])
    |otherwise = palindromeFinder first (second-1) limit palindromeList
    where potentialPalindrome = first * second

largestPalindromeProduct :: Integer -> Integer
largestPalindromeProduct number = head.reverse.sort $ (palindromeFinder number number number [])

ghc :: Integer -> Integer -> Integer
ghc n1 n2
    |n2 == 0 = n1
    |otherwise = ghc n2 (n1 `mod` n2)

lcm :: Integer -> Integer -> Integer
lcm number1 number2 = round ((fromInteger (number1 * number2)) / (fromInteger (ghc number1 number2)))

smallestNumberDivisibleByRange :: Integer -> Integer -> Integer
smallestNumberDivisibleByRange limit initial
    |limit == 1 = initial
    |otherwise = smallestNumberDivisibleByRange (pred limit) (Prelude.lcm limit initial)

squereAndSum :: Integer -> Integer -> Integer
squereAndSum total number = total + (number * number)

sumOfSquere :: Integer -> Integer
sumOfSquere number = foldl1 squereAndSum [1..number]

squereOfSum :: Integer -> Integer
squereOfSum number = sum * sum
                    where sum = foldl1 (+) [1..number]

diffOfSumOfSquereAndSquereOfSum :: Integer -> Integer
diffOfSumOfSquereAndSquereOfSum number = foldl1 (-) $ [squereOfSum, sumOfSquere] <*> [number]

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

toInt :: [Char] -> [Integer]
toInt string = map fromIntegral (map digitToInt string)

sanitizeSeries :: [Integer] -> Integer -> [[Integer]] -> [[Integer]]
sanitizeSeries numList chunkSize resultBucket
    |(<) (length numList) (fromIntegral chunkSize) = resultBucket
    |not (elem 0 chunk) = sanitizeSeries (tail numList) chunkSize (chunk : resultBucket)
    |otherwise = sanitizeSeries (tail numList) chunkSize (resultBucket)
    where chunk = take (fromIntegral chunkSize) numList

productOfNumberLists :: [[Integer]] -> Integer -> Integer
productOfNumberLists numLists multVal
    | length numLists == 0 = multVal
    | newVal > multVal = productOfNumberLists (tail numLists) newVal
    | otherwise = productOfNumberLists (tail numLists) multVal
     where newVal = foldl1 (*) (head numLists)

findNextElm :: [Integer] -> Integer -> Integer -> Integer
findNextElm numList index limit
    |nextIndex > limit && element == 0 = 1
    |otherwise =  element
    where element = numList !! (fromIntegral index)
          nextIndex = succ index

multValForCurrentFrame :: [Integer] -> Integer -> Integer -> Integer
multValForCurrentFrame numList index frameSize = foldl1 (*) (take (fromIntegral.pred $ frameSize) numList)

findNextIndexOfNonZeroValue :: [Integer] -> Integer -> Integer
findNextIndexOfNonZeroValue numList frameIndex
    |numList !! (fromIntegral frameIndex) == 0 = succ frameIndex
    |otherwise = findNextIndexOfNonZeroValue numList (pred frameIndex)

slice :: [Integer] -> Integer -> Integer -> [Integer]
slice numList start limit  = take (fromIntegral limit).(drop.fromIntegral $ start) $ numList

multipleOfChunkOfAList :: [Integer] -> Integer -> Integer -> Integer
multipleOfChunkOfAList numList frameIndex limit = foldl1 (*) (slice numList frameIndex limit)

multiplier :: [Integer] -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
multiplier numberList limit frameSize multVal currentVal frameIndex
    |(frameSize + frameIndex) > limit = multVal
    |currentMultVal == 0 = multiplier
      numberList
      limit frameSize
      multVal (multipleOfChunkOfAList numberList (findNextIndexOfNonZeroValue numberList frameIndex) (pred limit))
      (findNextIndexOfNonZeroValue numberList frameIndex)
    |currentMultVal > multVal = multiplier numberList limit frameSize currentMultVal currentVal nextIndex
    |otherwise =  multiplier numberList limit frameSize multVal currentVal nextIndex
    where currentMultVal = currentVal * (numberList !! ((fromIntegral frameIndex) + pred (fromIntegral frameSize)))
          currentVal = currentMultVal `div` lastElementForCurrentFrame
          nextIndex = succ frameIndex
          lastElementForCurrentFrame = findNextElm numberList frameIndex limit

productOfNumbers :: [Integer] -> Integer -> Integer -> Integer
productOfNumbers numberList frameSize multVal = multiplier numberList (fromIntegral (length numberList)) frameSize currentMultVal currentValForNextIter 1
    where currentMultVal = foldl1 (*) (take (fromIntegral frameSize) numberList)
          currentValForNextIter = currentMultVal `div` (numberList !! (fromIntegral (pred frameSize)))

pythagoreanTripletFinder :: Integer -> [[Integer]]
pythagoreanTripletFinder sum = [[a,b,c] | m <- [2..limit],
    n <- [1..(m-1)],
    let a = m^2 - n^2,
    let b = 2*m*n,
    let c = m^2 + n^2,
    a+b+c==sum]
    where limit = (floor.sqrt.fromIntegral) sum

pythagoreanTripletProduct :: Integer -> Integer
pythagoreanTripletProduct sum = product.head.pythagoreanTripletFinder $ sum

primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]
 where
  sieve (p:ps) xs = h ++ sieve ps [x | x <- t, x `rem` p /= 0]
                  where (h,~(_:t)) = span (< p*p) xs

sumOfPrimes :: Integer -> Integer
sumOfPrimes limit = foldl1 (+) (takeWhile (<limit) primes)

nThPrime :: Integer -> Integer
nThPrime number = primes !! fromIntegral (number-1)
