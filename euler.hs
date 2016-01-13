module Euler where

multipleOf3Or5 :: Integer -> Bool
multipleOf3Or5 a = if (a `mod` 3 == 0) || (a `mod` 5 == 0)
                        then True
                        else False

multipleOf17Or19 :: Integer -> Bool
multipleOf17Or19 a = if (a `mod` 17 == 0) || (a `mod` 19 == 0)
                        then True
                        else False

sumOfMultiPlesInAGivenRangeUnderACondition :: Integer -> Integer -> (Integer -> Bool) -> Integer
sumOfMultiPlesInAGivenRangeUnderACondition first second predicate = if first > second
                                                                        then 0
                                                                        else foldl (+) 0 (filter predicate [first..second])