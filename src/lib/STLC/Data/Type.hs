module STLC.Data.Type where

import STLC.Data.Name ( Name )


data Type
  = TFree Name
  | Fun Type Type
  deriving (Show, Eq)
