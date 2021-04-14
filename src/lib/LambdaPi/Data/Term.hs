module LambdaPi.Data.Term where

import LambdaPi.Data.Name



data ITerm
  = Ann CTerm CTerm
  | Star
  | Pi CTerm CTerm
  | Bound Int
  | Free Name
  | ITerm :@: CTerm
  deriving (Show, Eq)

data CTerm
  = Inf ITerm
  | Lam CTerm
  deriving (Show, Eq)

data Value
  = VLam (Value -> Value)
  | VStar
  | VPi Value (Value -> Value)
  | VNeutral Neutral

data Neutral
  = NFree Name
  | NApp Neutral Value

-- Create a value corresponding to a free variable.
vfree :: Name -> Value
vfree n = VNeutral (NFree n)