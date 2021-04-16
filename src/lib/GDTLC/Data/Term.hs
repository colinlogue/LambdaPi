module GDTLC.Data.Term where

import GDTLC.Data.Name ( Name )



data ITerm
  = Ann CTerm CTerm
  | Star
  | Pi CTerm CTerm
  | Bound Int
  | Free Name
  | ITerm :@: CTerm
  -- Nat
  | Nat
  | NatElim CTerm CTerm CTerm CTerm
  | Zero
  | Succ CTerm
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
  -- Nat
  | VNat
  | VZero
  | VSucc Value

data Neutral
  = NFree Name
  | NApp Neutral Value
  -- Nat
  | NNatElim Value Value Value Neutral

-- Create a value corresponding to a free variable.
vfree :: Name -> Value
vfree n = VNeutral (NFree n)