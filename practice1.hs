sumOfNums = sum [1..100]
modulus = mod 5 3
modulus1 = 5 `mod` 3
primeNumbers = 2 : 3 : 5 : 7 : []
morePrimes = [2,3,5,7,11,13,17,19]
primeLength = length morePrimes
reversePrimes = reverse morePrimes
isListEmpty = null morePrimes
isListEmpty1 = null []
firstPrime = morePrimes !! 0
firstPrimeOfEmptyList = [] !! 0 -- *** Exception: Prelude.(!!): index too large
lastPrime = last morePrimes
primeInit = init morePrimes
firstThreePrimes = take 3 morePrimes
removeFirstThreePrimes = drop 3 morePrimes
is7InList = elem 7 morePrimes
largestPrime = maximum morePrimes
smallestPrime = minimum morePrimes
productPrime = product morePrimes
listOfOneToTen = [1..10]
evenUpTo20 = [2,4..20]
alphabets = ['A','B'..'z']
infinite10 = [10,20..]
fithinfinite10 = infinite10 !! 49
many2s = take 100 (repeat 2)
many3s = replicate 10 3
many3s1 = 10 `replicate` 3
cycleList = take 100 (cycle [1,2,3,4,5,6,7,8,9,10])
listTimes2 =  [x * 2 | x <- [1..10]]
filteredListTimes3 =  [x * 3 | x <- [1..50], x * 3 <= 50]
divisibleBy9And13 = [x | x<- [1..500], mod x 9 == 0, x `mod` 13 ==0]
-- sortedList = sort [1,2,89,10,25,45] not working
sumOfLists = zipWith (+) [1,2,3,4,5] [1,2,3,4,5]
biggerThan5 = filter (>5) [1,2,3,4,5,6,7,8,9]
evensUpto20 = takeWhile (<=20) (filter (even) [2,4..])
reduceList = foldl (+) 0 [1,2,3,4,5]
reduceList1 = foldr (+) 0 [1,2,3,4,5]
power3List = [3^n | n <- [1..10]]
power3elEments = [n^3 | n <- [1..10]]
multTable = [[x * y | y <- [1..10]] | x <- [1..10]]
bobSmith = ("Bob Smith", 52)
bobName = fst bobSmith
bobAge =  snd bobSmith
names = ["Prasenjit", "Saheb"]
addresses = ["hell", "hell"]
namesAndAddresses = zip names addresses  
