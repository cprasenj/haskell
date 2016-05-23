module Euler11to20 where
import Data.List
import Data.Char (digitToInt)
import Data.Maybe

functionRepeater :: [[Int]] -> ([[Int]] -> [[Int]]) -> Int -> [[Int]]
functionRepeater list listOperator repeatTime =
    if repeatTime <= 0 || length list == 0
    then list
    else functionRepeater (listOperator list) listOperator (pred repeatTime)

createLinearChunks :: [Int] -> Int -> [[Int]]
createLinearChunks list chunkSize =
    if chunkSize <= 0
    then []
    else functionRepeater (map (\x -> take chunkSize (drop (fromJust (elemIndex x list)) list)) list) init (pred chunkSize)

-- createChunks :: [Int] -> Int ->
-- createLinearChunks :: [Int] -> Int -> [[Int]]


findLargestMultiple :: [[Int]] -> Int
findLargestMultiple [] = 0
findLargestMultiple listOfLists = maximum (map (\x -> foldl (*) 1 x) listOfLists)

findLargestLinearMultiple :: [Int] -> Int -> Int -> Int -> Int -> Int
findLargestLinearMultiple list chunkSize currentIndex initialVal maxVal =
    if chunkSize <= 0 || length list - startingChunkIndex  < chunkSize
    then maxVal
    else if currentVal == 0
    then findLargestLinearMultiple list chunkSize (succ currentIndex) initialVal maxVal
    else if multiple < maxVal || currentIndex < pred chunkSize
    then findLargestLinearMultiple list chunkSize nextIndex initialVal maxVal
    else findLargestLinearMultiple list chunkSize nextIndex nextInitialVal  multiple
    where currentVal = list !! currentIndex
          multiple = initialVal * currentVal
          nextIndex = succ currentIndex
          startingChunkIndex = currentIndex - pred chunkSize
          chunkHead =
            if (list !! startingChunkIndex == 0)
            then initialVal
            else list !! startingChunkIndex
          nextInitialVal = multiple `div` chunkHead


-- 1 < 2 ? "Yes" :? "No"


-- giveIndexInInterval ::  Int -> Int -> Int -> [Int] ->[Int]
-- giveIndexInInterval interval gridSize start initialList =
--     if start > gridSize * gridSize || (length initialList) > interval
--     then initialList
--     else interval gridSize (start + interval) (initialList ++ [start])

-- createVerticalChunks :: [Int] -> Int -> Int -> [[Int]]
-- createVerticalChunks list chunkSize gridSize =
--     if chunkSize <= 0
--     then []
--     else functionRepeater (map (\x -> take chunkSize (drop (fromJust (elemIndex x list)) list)) list) init (pred chunkSize)