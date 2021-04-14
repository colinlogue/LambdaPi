module STLC.Data.Value where

import STLC.Data.Name (Name)

data Value
  = VLam (Value -> Value)
  | VNeutral Neutral

data Neutral
  = NFree Name
  | NApp Neutral Value

-- Create a value corresponding to a free variable.
vfree :: Name -> Value
vfree n = VNeutral (NFree n)