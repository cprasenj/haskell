module Lession3 where

data StatusMessage = Nothing
  | Creation String
  | OnDelete String
  deriving Show

data Tree a = EmptyNode
  | Node a (Tree a) (Tree a)
  deriving Show

data Hidden = True
  | False
  deriving Show

data Task = String Hidden
  deriving Show

data TaskStatus = Active
  | Pending
  | Done
  deriving Show

data Visibility = All Hidden
  | TaskStatus Hidden
  deriving Show
