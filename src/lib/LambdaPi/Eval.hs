module LambdaPi.Eval where

import LambdaPi.Data.Term
import LambdaPi.Data.Env
import LambdaPi.Data.Name


evalI :: ITerm -> Env -> Value
evalI (Ann e _)  d = evalC e d
evalI (Free x)   _ = vfree x
evalI (Bound i)  d = d !! i
evalI (e :@: e') d = vapp (evalI e d) (evalC e' d)
evalI Star       _ = VStar
evalI (Pi t t')  d = VPi (evalC t d) (\x -> evalC t' (x:d))

vapp :: Value -> Value -> Value
vapp (VLam f)     v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

evalC :: CTerm -> Env -> Value
evalC (Inf i) d = evalI i d
evalC (Lam e) d = VLam (\x -> evalC e (x : d))

quote :: Int -> Value -> CTerm
quote i (VLam f)     = Lam (quote (i+1) (f (vfree (Quote i))))
quote i (VNeutral n) = Inf (neutralQuote i n)
quote _ VStar        = Inf Star
quote i (VPi v f) =
  Inf (Pi (quote i v) (quote (i+1) (f (vfree (Quote i)))))

neutralQuote :: Int -> Neutral -> ITerm
neutralQuote i (NFree x)  = boundfree i x
neutralQuote i (NApp n v) = neutralQuote i n :@: quote i v

boundfree :: Int -> Name -> ITerm
boundfree i (Quote k) = Bound (i - k - 1)
boundfree _ x         = Free x

