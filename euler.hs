module Euler where

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