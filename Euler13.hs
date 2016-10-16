module Euler13 where
import Data.List
import Data.Char (digitToInt)
import Data.Ix

addTwoList :: [Int] -> [Int] -> [Int]
addTwoList list1 list2
  |(length list2) > (length list1) = listAdder list2 list1 0 []
  |otherwise = listAdder list1 list2 0 []

listAdder :: [Int] -> [Int] -> Int -> [Int] -> [Int]
listAdder list1 list2 prevCarry bucket
  |length list1 == 0 = bucket
  |length list2 == 0 = listAdder (tail list1) list2 carry (remeinder : bucket)
  |otherwise = listAdder (tail list1) (tail list2) carry (remeinder : bucket)
  where summetion = x + y + prevCarry
        x = if length list1 > 0 then head list1 else 0
        y = if length list2 > 0 then head list2 else 0
        carry = if summetion > 10 then div summetion 10 else 0
        remeinder = if summetion > 10 then rem summetion 10 else 0

addBigNums :: [[Int]] -> Int
addBigNums [] = 0
addBigNums listOfBigNums = foldl1 addTwoList listOfBigNums
