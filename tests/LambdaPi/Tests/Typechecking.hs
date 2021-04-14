module LambdaPi.Tests.Typechecking where

import LambdaPi.Data.Term
import LambdaPi.Data.Name
import Test.Hspec
import LambdaPi.Data.Context
import LambdaPi.Typecheck
import LambdaPi.Eval
import Data.Bifunctor (Bifunctor(first))

var :: String -> ITerm
var x = Free (Global x)

var' :: String -> CTerm
var' = Inf . var

type Context' = [(String, Type)]

liftContext :: Context' -> Context
liftContext = fmap (first Global)
hasType :: ITerm -> ITerm -> Context' -> Expectation
hasType e t g =
  quote 0 <$> typeI 0 (liftContext g) e `shouldBe` Right (Inf t)

inContext :: (Context' -> Expectation) -> Context' -> Expectation
inContext = id

typecheckingTests :: IO ()
typecheckingTests = hspec $ do
  describe "typeI" $ do

    it "checks * as *" $
      Star `hasType` Star `inContext` []

    it "checks (t : *) as * in context [(t,*)]" $
      Ann (var' "t") (Inf Star) `hasType` Star
        `inContext` [("t", VStar)]