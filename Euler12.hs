module Euler12 where
import Data.List
import Data.Char (digitToInt)

additionTill :: Int -> Int
additionTill number = foldl (+) 0 [1..number]

findDivisors :: Int -> Int ->[Int] -> [Int]
findDivisors number index initList
    |or $ [(number <= 0),  (index * 2 >  number)] = initList ++ [number]
    |number `rem` index == 0 = findDivisors number (succ index) initList ++ [index]
    |otherwise = findDivisors number (succ index) initList

triangleNumbersWithDivisors :: Int -> Int -> Int -> Int
triangleNumbersWithDivisors additionTillIndex numberIndex numberOfDivisors =
    if length (findDivisors additionTillIndex start []) == numberOfDivisors
    then additionTillIndex
    else if length (findDivisors additionTillIndex start []) == numberOfDivisors
    then triangleNumbersWithDivisors additionTillIndex (pred nextNumberIndex)  numberOfDivisors
    else triangleNumbersWithDivisors (additionTillIndex + nextNumberIndex) nextNumberIndex  numberOfDivisors
    where start = 1
          nextNumberIndex = succ numberIndex

-- triangleNumbersWithDivisors :: Int -> Int -> Int
-- triangleNumbersWithDivisors index numberOfDivisors =
--     if divisorsCount == numberOfDivisors
--     then additionTillIndex
--     else if divisorsCount > numberOfDivisors
--     then triangleNumbersWithDivisors (index - (jumpFactor * 2 - 1)) numberOfDivisors
--     else if index < 10
--     then triangleNumbersWithDivisors (index + 1) numberOfDivisors
--     else triangleNumbersWithDivisors (index + jumpFactor)  numberOfDivisors
--     where start = 1
--           nextNumberIndex = succ index
--           additionTillIndex = additionTill index
--           jumpFactor = 10
--           divisorsCount = length (findDivisors additionTillIndex start [])
