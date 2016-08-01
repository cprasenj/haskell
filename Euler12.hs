module Euler12 where
import Data.List
import Data.Char (digitToInt)
import Data.Ix

findDivisors :: Int -> [Int]
findDivisors number = [x | x <- [1..number], (== 0).(rem number) $ x]

foldTillNumber :: Int -> Int
foldTillNumber upperBound
  |upperBound <= 0 = 0
  |otherwise = (foldl1 (+)).range $ (1, upperBound)

triangularNumberWithDivisor :: Int -> Int
triangularNumberWithDivisor numberOfdivisors = foldTillNumber upperBound
    where upperBound = head [x | x <- [1..], ((>= numberOfdivisors).length.findDivisors.foldTillNumber $ x)]
