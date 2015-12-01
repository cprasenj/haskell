module LogAnalysis where
import Log
import Data.List
import Data.List.Split

parseMessage :: String -> LogMessage
parseMessage aLine
  | aLine !! 0 == 'E' = LogMessage (Error (read (splittedLine !! 1) :: Int)) (read (splittedLine !! 2) :: Int) (intercalate " " (drop 3 splittedLine))
  | aLine !! 0 == 'I' = LogMessage Info (read (splittedLine !! 1) :: Int) (intercalate " " (drop 2 splittedLine))
  | aLine !! 0 == 'W' = LogMessage Warning (read (splittedLine !! 1) :: Int) (intercalate " " (drop 2 splittedLine))
  | otherwise = Unknown aLine
  where splittedLine = splitOn " " aLine

parse :: String -> [LogMessage]
parse fileString
  |length fileString == 0 = error "empty file"
  |otherwise = map parseMessage linesOfFile
  where linesOfFile = lines fileString

insert1 :: (Ord LogMessage) => MessageTree -> LogMessage -> MessageTree
insert1 Leaf x = Node Leaf x Leaf
insert1 (Node t1 v t2) x
	| v == x = Node t1 v t2
	| v  < x = Node t1 v (insert1 t2 x)
	| v  > x = Node (insert1 t1 x) v t2
