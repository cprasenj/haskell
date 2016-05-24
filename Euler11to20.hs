module Euler11to20 where
import Data.List
import Data.Char (digitToInt)

functionRepeater :: [[Int]] -> ([[Int]] -> [[Int]]) -> Int -> [[Int]]
functionRepeater list listOperator repeatTime =
    if repeatTime <= 0 || length list == 0
    then list
    else functionRepeater (listOperator list) listOperator (pred repeatTime)

createChunksHorizontally :: [Int] -> Int -> [[Int]] -> [[Int]]
createChunksHorizontally list chunkSize resultContainer =
    if chunkSize <= 0
    then resultContainer
    else if length list < chunkSize
    then functionRepeater resultContainer init (pred chunkSize)
    else if listHead == 0
    then createChunksHorizontally restOfList chunkSize resultContainer
    else createChunksHorizontally restOfList chunkSize resultContainer ++ [take chunkSize list]
    where listHead = head list
          restOfList = tail list

createChunkForInterval :: [Int] -> Int -> Int -> [Int] -> [Int]
createChunkForInterval list chunkSize gridLength resultContainer =
    if length list == 0 || chunkSize == 0
    then resultContainer
    else createChunkForInterval (drop gridLength list) (pred chunkSize) gridLength resultContainer ++ [head list]


createChunksVertically :: [Int] -> Int -> Int -> [[Int]] -> [[Int]]
createChunksVertically list chunkSize gridLength resultContainer =
    if chunkSize <= 0
    then resultContainer
    else if length list < chunkSize
    then functionRepeater resultContainer init (pred chunkSize)
    else if listHead == 0
    then createChunksVertically restOfList chunkSize gridLength resultContainer
    else createChunksVertically restOfList chunkSize gridLength resultContainer ++ [createChunkForInterval list chunkSize gridLength []]
    where listHead = head list
          restOfList = tail list

createLinearChunks :: [Int] -> Int -> [[Int]]
createLinearChunks list chunkSize = createChunksHorizontally list chunkSize []

createVerticalChunks :: [Int] -> Int -> Int -> [[Int]]
createVerticalChunks list chunkSize gridLength = createChunksVertically list chunkSize gridLength []

findLargestMultiple :: [[Int]] -> Int
findLargestMultiple [] = 0
findLargestMultiple listOfLists = maximum (map (\x -> foldl (*) 1 x) listOfLists)

findLargestLinearMultiple :: [Int] -> Int -> Int -> Int -> Int -> Int
findLargestLinearMultiple list chunkSize currentIndex initialVal maxVal =
    if chunkSize <= 0 || length list - startingChunkIndex  < chunkSize
    then maxVal
    else if currentVal == 0
    then findLargestLinearMultiple list chunkSize nextIndex initialVal maxVal
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