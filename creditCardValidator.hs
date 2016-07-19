module CreditCardValidator where

import Practice

toDigits :: Integer -> [Integer]
toDigits x
  |x <= 0 = []
  |otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverseList . toDigits $ x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x*2]
doubleEveryOther (x:xs) = doubleEveryOther (tail xs) ++ [head xs] ++ [x*2]

sumDigitsOfANumber :: Integer -> Integer
sumDigitsOfANumber a
  |a < 10 = a
  |otherwise = foldl1 (+) $ [(div a), (mod a)] <*> [10]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (a:as) = sumDigitsOfANumber(a) + sumDigits(as)

validate :: Integer -> Bool
validate a = (sumDigits . doubleEveryOther . toDigits $ a) `mod` 10 == 0
