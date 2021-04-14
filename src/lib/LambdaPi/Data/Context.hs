module LambdaPi.Data.Context where

import LambdaPi.Data.Term
import LambdaPi.Data.Name



type Type = Value
type Context = [(Name,Type)]