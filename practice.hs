module Practice where

mylast :: [a] -> a
mylast [] = error "No last element found"
mylast [a] = a
mylast (a : as) = mylast as

mySecondLast :: [a] -> a
mySecondLast [] = error "No element found"
mySecondLast [x] = error " array has only one element"
mySecondLast [x, y] = x
mySecondLast (_ : xs) = mySecondLast xs

findKthElement :: [a] -> Integer -> a
findKthElement [] y = error "blah"
findKthElement (x:_) 1 = x
findKthElement (x:xs) k
	| k<=0 = error "IndexError"
	| otherwise = findKthElement xs (k-1)

findELementCount :: [a] -> Integer
findELementCount []  = 0
findELementCount (x:xs) = 1 + findELementCount xs

reverseList :: [a] -> [a]
reverseList [x]  = [x]
reverseList (x:xs) = reverseList xs ++ (x:[])

isEqualList :: (Ord a) => [a] -> [a] -> Bool
isEqualList [] [] = True
isEqualList [] [a] = False
isEqualList [a] [] = False
isEqualList (x:xs) (y:ys)
  |x==y = isEqualList xs ys
  |otherwise = False

isPalindrome :: (Ord a) => [a] -> Bool
isPalindrome list = isEqualList list (reverseList list)

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

compressor :: (Ord a) => [a] -> [a]
compressor [] = []
compressor [a] = [a]
compressor (x:xs)
	|any(x==) xs == True = compressor(xs)
	|otherwise = compressor xs ++ (x:[])

compress :: (Ord a) => [a] -> [a]
compress a = reverseList(compressor(a))

createPack :: (Ord a) => [a] -> a -> [a]
createPack [] a = []
createPack x a
	|length(compress x) == 1 = x
createPack (x:xs) a
	|x==a = createPack (xs ++ (x:[])) a
	|otherwise = createPack xs a

pack :: (Ord a) => [a] -> [[a]]
pack a = map (createPack a) (compress a)

countList :: [a] -> (Int, a)
countList a = (length a, head a)

encode :: (Ord a) => [a] -> [(Int,a)]
encode a = map countList (pack a)
