module Euler11 where
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

createChunksDiagonally :: [Int] -> Int -> Int -> [[Int]] -> [[Int]]
createChunksDiagonally list chunkSize gridLength resultContainer =
    if chunkSize <= 0
    then resultContainer
    else if length list < chunkSize
    then resultContainer
    else if listHead == 0
    then createChunksDiagonally restOfList chunkSize gridLength resultContainer
    else createChunksDiagonally restOfList chunkSize gridLength resultContainer
        ++ [createChunkForInterval list chunkSize (succ gridLength) []]
        ++ [createChunkForInterval list chunkSize (pred gridLength) []]
    where listHead = head list
          restOfList = tail list

createLinearChunks :: [Int] -> Int -> [[Int]]
createLinearChunks list chunkSize = filter (\x -> length x == chunkSize) (createChunksHorizontally list chunkSize [])

createVerticalChunks :: [Int] -> Int -> Int -> [[Int]]
createVerticalChunks list chunkSize gridLength = filter (\x -> length x == chunkSize) (createChunksVertically list chunkSize gridLength [])

createDiagonalChunks :: [Int] -> Int -> Int -> [[Int]]
createDiagonalChunks list chunkSize gridLength = filter (\x -> length x == chunkSize) (createChunksDiagonally list chunkSize gridLength [])

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

largestProduct :: [Int] -> Int -> Int -> Int
largestProduct list chunkSize gridLength= maximum [
    (maximum (map (\x -> foldl (*) 1 x) (createLinearChunks list chunkSize))),
    (maximum (map (\x -> foldl (*) 1 x) (createVerticalChunks list chunkSize gridLength))),
    (maximum (map (\x -> foldl (*) 1 x) (createDiagonalChunks list chunkSize gridLength)))]