module STLC.Data.Term where

import STLC.Data.Name ( Name )
import STLC.Data.Type ( Type )

infixl 4 :@:

data ITerm
  = Ann CTerm Type
  | Bound Int
  | Free Name
  | ITerm :@: CTerm
  deriving (Show, Eq)

data CTerm
  = Inf ITerm
  | Lam CTerm
  deriving (Show, Eq)

