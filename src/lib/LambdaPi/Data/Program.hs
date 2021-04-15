module LambdaPi.Data.Program where

import LambdaPi.Data.Term


data Stmt
  = Let String ITerm
  | Expr ITerm
  deriving (Eq, Show)

newtype Program
  = Program [Stmt]