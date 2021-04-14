module STLC.Data.Context where

import STLC.Data.Name (Name)
import STLC.Data.Type ( Type )




data Kind
  = Star
  deriving (Show)

data Info
  = HasKind Kind
  | HasType Type
  deriving (Show)

type Context = [(Name, Info)]