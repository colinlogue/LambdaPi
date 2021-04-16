module GDTLC.Eval where

import GDTLC.Data.Term
import GDTLC.Data.Env
import GDTLC.Data.Name


evalI :: ITerm -> Env -> Value
evalI (Ann e _)  d = evalC e d
evalI (Free x)   _ = vfree x
evalI (Bound i)  d = d !! i
evalI (e :@: e') d = vapp (evalI e d) (evalC e' d)
evalI Star       _ = VStar
evalI (Pi t t')  d = VPi (evalC t d) (\x -> evalC t' (x:d))
-- Nat
evalI Nat _ = VNat
evalI Zero _ = VZero
evalI (Succ k) d = VSucc (evalC k d)
evalI (NatElim m mz ms k) d =
  let
    mzVal = evalC mz d
    msVal = evalC ms d
    rec kVal =
      case kVal of
        VZero -> mzVal
        VSucc l -> msVal `vapp` l `vapp` rec l
        VNeutral k -> VNeutral
          (NNatElim (evalC m d) mzVal msVal k)
  in rec (evalC k d)

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
quote _ VNat = Inf Nat
quote _ VZero = Inf Zero
quote i (VSucc k) = Inf $ Succ (quote i k)

neutralQuote :: Int -> Neutral -> ITerm
neutralQuote i (NFree x)  = boundfree i x
neutralQuote i (NApp n v) = neutralQuote i n :@: quote i v
neutralQuote i (NNatElim vm vmz vms nk) =
  NatElim (quote i vm) (quote i vmz) (quote i vms) (Inf (neutralQuote i nk))

boundfree :: Int -> Name -> ITerm
boundfree i (Quote k) = Bound (i - k - 1)
boundfree _ x         = Free x


instance Eq Value where
  x == y = quote 0 x == quote 0 y

instance Show Value where
  show = show . quote 0