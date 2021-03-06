module Hanoi where
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> peg -> peg -> peg -> [(peg, peg)]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
