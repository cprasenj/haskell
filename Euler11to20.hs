module Euler11to20 where
import Data.List
import Data.Char (digitToInt)
import Data.Maybe

-- largestProductInAGridUp ::


functionRepeater :: [[Int]] -> ([[Int]] -> [[Int]]) -> Int -> [[Int]]
functionRepeater list listOperator repeatTime =
    if repeatTime <= 0 || length list == 0
    then list
    else functionRepeater (listOperator list) listOperator (pred repeatTime)

-- giveIndexInInterval ::  Int -> Int -> Int -> [Int] ->[Int]
-- giveIndexInInterval interval gridSize start initialList =
--     if start > gridSize * gridSize || (length initialList) > interval
--     then initialList
--     else interval gridSize (start + interval) (initialList ++ [start])

createLinearChunks :: [Int] -> Int -> [[Int]]
createLinearChunks list chunkSize =
    if chunkSize <= 0
    then []
    else functionRepeater (map (\x -> take chunkSize (drop (fromJust (elemIndex x list)) list)) list) init (pred chunkSize)

-- createVerticalChunks :: [Int] -> Int -> Int -> [[Int]]
-- createVerticalChunks list chunkSize gridSize =
--     if chunkSize <= 0
--     then []
--     else functionRepeater (map (\x -> take chunkSize (drop (fromJust (elemIndex x list)) list)) list) init (pred chunkSize)





