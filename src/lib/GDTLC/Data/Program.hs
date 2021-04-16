module GDTLC.Data.Program where

import GDTLC.Data.Term


data Stmt
  = Let String ITerm
  | Expr ITerm
  | Include String
  deriving (Eq, Show)

newtype Program
  = Program [Stmt]