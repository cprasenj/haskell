module Euler where
import Data.List

multipleOf3Or5 :: Integer -> Bool
multipleOf3Or5 a =
    if (a `mod` 3 == 0) || (a `mod` 5 == 0)
        then True
        else False

multipleOf17Or19 :: Integer -> Bool
multipleOf17Or19 a =
    if (a `mod` 17 == 0) || (a `mod` 19 == 0)
        then True
        else False

sumOfMultiPlesInAGivenRangeUnderACondition :: Integer -> Integer -> (Integer -> Bool) -> Integer
sumOfMultiPlesInAGivenRangeUnderACondition first second predicate =
    if first > second
        then 0
        else foldl (+) 0 (filter predicate [first..second])

fibonachi :: Integer -> Integer -> Integer ->[Integer]
fibonachi first second limit =
    if first > second || limit <= first
        then []
    else [first] ++ fibonachi second (first + second) limit

isEven :: Integer -> Bool
isEven number = number `mod` 2 == 0

createSumOfFilteredList :: Integer -> Integer -> Integer -> (Integer -> Integer-> Integer -> [Integer]) -> (Integer -> Bool) -> Integer
createSumOfFilteredList start end limit listCreator predicate  = foldl (+) 0 (filter predicate (listCreator start end limit))

sieve :: (Integral a) => [a] -> [a]
sieve (n:ns) = n : sieve ns'
  where ns' = filter ((/= 0) . flip rem n) (n:ns)

isDivisible :: (Integral a) => a -> a -> Bool
isDivisible number1 number2 = number1 `mod` number2 == 0


largestPrimeFactor :: Integer -> Integer
largestPrimeFactor number = head (reverse (filter predicate (takeWhile (< round (sqrt(fromIntegral number)/2)) (sieve [2..]))))
                            where predicate = isDivisible number

reverseInt :: Integer -> Integer
reverseInt x = (*) (signum x) . read . reverse . show . abs  $ x

isPalindrome :: Integer -> Bool
isPalindrome number = number == reverseInt number

palindromeFinder :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
palindromeFinder first second limit palindromeList =
                                               if first == 1 && second == 1
                                               then palindromeList
                                               else if second == 1 && isPalindrome potentialPalindrome
                                               then palindromeFinder (first-1) limit limit (palindromeList ++ [potentialPalindrome])
                                               else if second == 1
                                               then palindromeFinder (first-1) limit limit palindromeList
                                               else if isPalindrome potentialPalindrome
                                               then palindromeFinder first (second-1) limit (palindromeList ++ [potentialPalindrome])
                                               else palindromeFinder first (second-1) limit palindromeList
                                               where potentialPalindrome = first * second

largestPalindromeProduct :: Integer -> Integer
largestPalindromeProduct number = head (reverse (sort (palindromeFinder number number number [])))

ghc :: Integer -> Integer -> Integer
ghc number1 number2 = if number2 == 0
                      then number1
                      else ghc number2 (number1 `mod` number2)

lcm :: Integer -> Integer -> Integer
lcm number1 number2 = round ((fromInteger (number1 * number2)) / (fromInteger (ghc number1 number2)))

smallestNumberDivisibleByRange :: Integer -> Integer -> Integer
smallestNumberDivisibleByRange limit initial = if limit == 1
                                           then initial
                                           else smallestNumberDivisibleByRange (limit-1) (Prelude.lcm limit initial)

