{-# LANGUAGE LambdaCase #-}

module STLC.Typecheck where

import STLC.Data.Context
import STLC.Data.Term
import STLC.Data.Type
import Data.Functor (void)
import Control.Monad (unless)
import STLC.Data.Name



type Result a = Either String a

throwError :: String -> Result a
throwError = Left

fromMaybe :: String -> Maybe a -> Result a
fromMaybe _  (Just x) = Right x
fromMaybe msg Nothing = Left msg


kindC :: Context -> Type -> Kind -> Result ()
kindC g (TFree x) Star =
  void $ fromMaybe "unknown identifier" $ lookup x g
kindC g (Fun k k') Star =
  kindC g k Star *> kindC g k' Star

typeI :: Int -> Context -> ITerm -> Result Type
typeI i g (Ann e t) =
  kindC g t Star >> typeC i g e t >> pure t
typeI _ g (Free x) =
  fromMaybe "unknown identifier" (lookup x g) >>= \case
    HasType t -> Right t
typeI i g (e :@: e') =
  typeI i g e >>= \case
    Fun t t' -> typeC i g e' t >> pure t'
    _        -> throwError "illegal application"

typeC :: Int -> Context -> CTerm -> Type -> Result ()
typeC i g (Inf e) t =
  typeI i g e >>= \t' -> unless (t == t') (throwError "type mismatch")
typeC i g (Lam e) (Fun t t') =
  typeC
    (i + 1)
    ((Local i, HasType t) : g)
    (substC 0 (Free (Local i)) e)
    t'
typeC _ _ _ _ = throwError "type mismatch"

substI :: Int -> ITerm -> ITerm -> ITerm
substI i r (Ann e t)  = Ann (substC i r e) t
substI i r (Bound j)  = if i == j then r else Bound j
substI _ _ (Free y)   = Free y
substI i r (e :@: e') = substI i r e :@: substC i r e'

substC :: Int -> ITerm -> CTerm -> CTerm
substC i r (Inf e) = Inf (substI i r e)
substC i r (Lam e) = Lam (substC (i + 1) r e)