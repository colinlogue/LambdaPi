module STLC.Data.Program where

import STLC.Data.Term


newtype Ident = Ident String

data Stmt
  = Let Ident ITerm

newtype Program
  = Program [Stmt]