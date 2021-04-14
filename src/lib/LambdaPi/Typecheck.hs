{-# LANGUAGE LambdaCase #-}

module LambdaPi.Typecheck where

import LambdaPi.Data.Context
import LambdaPi.Data.Term
import Data.Functor (void)
import Control.Monad (unless)
import LambdaPi.Data.Name
import LambdaPi.Eval



type Result a = Either String a

throwError :: String -> Result a
throwError = Left

fromMaybe :: String -> Maybe a -> Result a
fromMaybe _  (Just x) = Right x
fromMaybe msg Nothing = Left msg


typeI :: Int -> Context -> ITerm -> Result Type
typeI i g (Ann e p) =
  let t = evalC p []
  in typeC i g p VStar >> typeC i g e t >> pure t
typeI _ _ Star = pure VStar
typeI _ g (Free x) =
  case lookup x g of
    Just t  -> pure t
    Nothing -> throwError "unknown identifier"
typeI i g (e :@: e') =
  typeI i g e >>= \case
    VPi t t' -> typeC i g e' t >> pure (t' (evalC e' []))
typeI i g (Pi p p') =
  do
    typeC i g p VStar
    let t = evalC p []
    typeC (i + 1) ((Local i,t):g) (substC 0 (Free (Local i)) p') VStar
    pure VStar
typeI _ _ (Bound _) = throwError "bound variable outside of context"

typeC :: Int -> Context -> CTerm -> Type -> Result ()
typeC i g (Inf e) v =
  typeI i g e >>= \v' ->
  unless (quote 0 v == quote 0 v') (throwError "type mismatch")
typeC i g (Lam e) (VPi t t') =
  typeC
    (i + 1)
    ((Local i, t) : g)
    (substC 0 (Free (Local i)) e)
    (t' (vfree (Local i)))
typeC _ _ _ _ = throwError "type mismatch"



substI :: Int -> ITerm -> ITerm -> ITerm
substI i r (Ann e t)  = Ann (substC i r e) (substC i r t)
substI i r (Bound j)  = if i == j then r else Bound j
substI _ _ (Free y)   = Free y
substI i r (e :@: e') = substI i r e :@: substC i r e'
substI _ _ Star       = Star
substI i r (Pi t t')  = Pi (substC i r t) (substC (i + 1) r t')

substC :: Int -> ITerm -> CTerm -> CTerm
substC i r (Inf e) = Inf (substI i r e)
substC i r (Lam e) = Lam (substC (i + 1) r e)