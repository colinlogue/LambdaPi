{-# LANGUAGE LambdaCase #-}

module GDTLC.Typecheck where

import GDTLC.Data.Context
import GDTLC.Data.Term
import Control.Monad (unless)
import GDTLC.Data.Name
import GDTLC.Eval
import Data.Bifunctor
import GDTLC.Printer


type TypeError = [String]
type Result a = Either TypeError a

throwError :: String -> Result a
throwError = first (:[]) . Left

fromMaybe :: String -> Maybe a -> Result a
fromMaybe _  (Just x) = Right x
fromMaybe msg Nothing = Left [msg]

infix 2 #
(#) :: Result a -> String -> Result a
r # msg = first (msg:) r

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
    _ -> throwError "illegal application"
typeI i g (Pi p p') =
  do
    typeC i g p VStar
      # printCTerm p ++ " does not have type *"
    let t = evalC p []
    typeC (i + 1) ((Local i,t):g) (substC 0 (Free (Local i)) p') VStar
      # printCTerm p' ++ " does not have type *"
    pure VStar
typeI _ _ (Bound _) = throwError "bound variable outside of context"
-- Nat
typeI _ _ Nat = pure VStar
typeI _ _ Zero = pure VNat
typeI i g (Succ k) = typeC i g k VNat >> pure VNat
typeI i g (NatElim m mz ms k) =
  do
    typeC i g m (VPi VNat (const VStar))
    let mVal = evalC m []
    typeC i g mz (mVal `vapp` VZero)
    typeC i g ms (VPi VNat (\l -> VPi (mVal `vapp` l) (\_ -> mVal `vapp` VSucc l)))
    typeC i g k VNat
    let kVal = evalC k []
    pure (mVal `vapp` kVal)

typeC :: Int -> Context -> CTerm -> Type -> Result ()
typeC i g (Inf e) v =
  typeI i g e >>= \v' ->
  unless (quote 0 v == quote 0 v') $
    throwError $ "type mismatch: " ++ printTerm e ++ " has inferred type "
      ++ printValue 0 v' ++ ", which does not match " ++ printValue 0 v
typeC i g (Lam e) (VPi t t') =
  typeC
    (i + 1)
    ((Local i, t) : g)
    (substC 0 (Free (Local i)) e)
    (t' (vfree (Local i)))
typeC _ _ (Lam _) _ = throwError "lambda does not have pi type"

substI :: Int -> ITerm -> ITerm -> ITerm
substI i r (Ann e t)  = Ann (substC i r e) (substC i r t)
substI i r (Bound j)  = if i == j then r else Bound j
substI _ _ (Free y)   = Free y
substI i r (e :@: e') = substI i r e :@: substC i r e'
substI _ _ Star       = Star
substI i r (Pi t t')  = Pi (substC i r t) (substC (i + 1) r t')
-- Nat
substI _ _ Nat = Nat
substI _ _ Zero = Zero
substI i r (Succ k) = Succ (substC i r k)
substI i r (NatElim m mz ms k) =
  let
    m'  = substC i r m
    mz' = substC i r mz
    ms' = substC i r ms
    k'  = substC i r k
  in NatElim m' mz' ms' k'

substC :: Int -> ITerm -> CTerm -> CTerm
substC i r (Inf e) = Inf (substI i r e)
substC i r (Lam e) = Lam (substC (i + 1) r e)



type Env = [(String, Type, Value)]

toContext :: Env -> Context
toContext = fmap (\(x,t,_) -> (Global x,t))

-- natElimType :: Type
-- natElimType =
--   VPi mType
--     (\m -> VPi (mzType m)
--     (\_ -> VPi (msType m)
--     (\_ -> VPi VNat
--     (\k -> m `vapp` k))))
--   where
--     mType :: Type
--     mType = VPi VNat (const VStar)

--     mzType :: Type -> Type
--     mzType m = m `vapp` VZero

--     msType :: Type -> Type
--     msType m = VPi VNat (\l -> VPi (m `vapp` l) (\_ -> m `vapp` VSucc l))

-- baseEnv :: Env
-- baseEnv =
--   [ ("natElim", natElimType,
--       VLam (\vm -> VLam (\vmz -> VLam (\vms -> VLam (\vk -> do
--         let m = quote 0 vm
--         let mz = quote 0 vmz
--         let ms = quote 0 vms
--         let k = quote 0 vk
--         evalI (NatElim m mz ms k) [])))))
--   , ("succ", VPi VNat (const VNat), VLam VSucc)
--   , ("zero", VNat, VZero)
--   , ("nat", VStar, VNat)
--   ]

baseEnv :: Env
baseEnv = []

baseContext :: Context
baseContext = toContext baseEnv