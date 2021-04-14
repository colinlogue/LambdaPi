module LambdaPi.Data.Name where


data Name
  = Global String
  | Local Int
  | Quote Int
  deriving (Show, Eq)