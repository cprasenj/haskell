module Euler11 where
import Data.List
import Data.Char (digitToInt)

functionRepeater :: [[Int]] -> ([[Int]] -> [[Int]]) -> Int -> [[Int]]
functionRepeater list listOperator repeatTime
    |repeatTime <= 0 || length list == 0 = list
    |otherwise = functionRepeater (listOperator list) listOperator (pred repeatTime)

createChunksHorizontally :: [Int] -> Int -> [[Int]] -> [[Int]]
createChunksHorizontally list chunkSize resultContainer
    |chunkSize <= 0 = resultContainer
    |length list < chunkSize = functionRepeater resultContainer init . pred $ chunkSize
    |listHead == 0 = createChunksHorizontally restOfList chunkSize resultContainer
    |otherwise = createChunksHorizontally restOfList chunkSize resultContainer ++ [take chunkSize list]
    where listHead = head list
          restOfList = tail list

createChunkForInterval :: [Int] -> Int -> Int -> [Int] -> [Int]
createChunkForInterval list chunkSize gridLength resultContainer
    |or $ [(== 0)] <*> [length list, chunkSize] = resultContainer
    |otherwise = createChunkForInterval (drop gridLength list) (pred chunkSize) gridLength resultContainer ++ [head list]

createChunksVertically :: [Int] -> Int -> Int -> [[Int]] -> [[Int]]
createChunksVertically list chunkSize gridLength resultContainer
    |chunkSize <= 0 = resultContainer
    |length list < chunkSize = functionRepeater resultContainer init (pred chunkSize)
    |listHead == 0 = createChunksVertically restOfList chunkSize gridLength resultContainer
    |otherwise = createChunksVertically restOfList chunkSize gridLength resultContainer ++ [createChunkForInterval list chunkSize gridLength []]
    where listHead = head list
          restOfList = tail list

createChunksDiagonally :: [Int] -> Int -> Int -> [[Int]] -> [[Int]]
createChunksDiagonally list chunkSize gridLength resultContainer
    |chunkSize <= 0 = resultContainer
    |length list < chunkSize = resultContainer
    |listHead == 0 = createChunksDiagonally restOfList chunkSize gridLength resultContainer
    |otherwise = createChunksDiagonally restOfList chunkSize gridLength resultContainer
        ++ [createChunkForInterval list chunkSize (succ gridLength) []]
        ++ [createChunkForInterval list chunkSize (pred gridLength) []]
    where listHead = head list
          restOfList = tail list

createLinearChunks :: [Int] -> Int -> [[Int]]
createLinearChunks list chunkSize = [x | x <- (createChunksHorizontally list chunkSize []), length x == chunkSize]

createVerticalChunks :: [Int] -> Int -> Int -> [[Int]]
createVerticalChunks list chunkSize gridLength = [x | x <- (createChunksVertically list chunkSize gridLength []), length x == chunkSize]

createDiagonalChunks :: [Int] -> Int -> Int -> [[Int]]
createDiagonalChunks list chunkSize gridLength = [x | x <- (createChunksDiagonally list chunkSize gridLength []), length x == chunkSize]

findLargestMultiple :: [[Int]] -> Int
findLargestMultiple [] = 0
findLargestMultiple listOfLists = maximum (map (\x -> foldl1 (*) x) listOfLists)

findLargestLinearMultiple :: [Int] -> Int -> Int -> Int -> Int -> Int
findLargestLinearMultiple list chunkSize currentIndex initialVal maxVal
    |or $ [chunkSize <= 0, length list - startingChunkIndex  < chunkSize ] = maxVal
    |currentVal == 0 = findLargestLinearMultiple list chunkSize nextIndex initialVal maxVal
    |or $ [multiple < maxVal, currentIndex < pred chunkSize] =
      findLargestLinearMultiple list chunkSize nextIndex initialVal maxVal
    |otherwise = findLargestLinearMultiple list chunkSize nextIndex nextInitialVal  multiple
    where currentVal = list !! currentIndex
          multiple = initialVal * currentVal
          nextIndex = succ currentIndex
          startingChunkIndex = currentIndex - pred chunkSize
          chunkHead
            |(list !! startingChunkIndex == 0) = initialVal
            |otherwise = list !! startingChunkIndex
          nextInitialVal = multiple `div` chunkHead

largestProduct :: [Int] -> Int -> Int -> Int
largestProduct list chunkSize gridLength= maximum [
    (maximum (map (\x -> foldl1 (*) x) (createLinearChunks list chunkSize))),
    (maximum (map (\x -> foldl1 (*) x) (createVerticalChunks list chunkSize gridLength))),
    (maximum (map (\x -> foldl1 (*) x) (createDiagonalChunks list chunkSize gridLength)))]
