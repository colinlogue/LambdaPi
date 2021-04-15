module LambdaPi.Tests.Typechecking where

import LambdaPi.Data.Term
import LambdaPi.Data.Name
import Test.Hspec
import LambdaPi.Data.Context
import LambdaPi.Typecheck
import LambdaPi.Eval
import Data.Bifunctor (Bifunctor(first))
import LambdaPi.Parser (parseTerm, parseCTerm)

var :: String -> ITerm
var x = Free (Global x)

var' :: String -> CTerm
var' = Inf . var

type Context' = [(String, Type)]

liftContext :: Context' -> Context
liftContext = fmap (first Global)

infersType :: ITerm -> ITerm -> Context' -> Expectation
infersType e t g =
  quote 0 <$> typeI 0 (liftContext g) e `shouldBe` Right (Inf t)

inContext :: (Context' -> Expectation) -> Context' -> Expectation
inContext = id

checksType :: CTerm -> ITerm -> Context' -> Expectation
checksType e t g =
  typeC 0 (liftContext g) e (evalI t []) `shouldBe` Right ()

checksType' :: String -> String -> Context' -> Expectation
checksType' e t g =
  let Right e' = parseCTerm e in
  let Right t' = parseTerm t in
    checksType e' t' g

typecheckingTests :: IO ()
typecheckingTests = hspec $ do
  
  describe "typeI" $ do

    it "infers * to be type *" $
      Star `infersType` Star `inContext` []

    it "infers (t : *) to be type * in context [(t,*)]" $
      Ann (var' "t") (Inf Star) `infersType` Star
        `inContext` [("t", VStar)]
    
    it "infers (forall (t : *), t -> t) as *" $
      case parseTerm "forall (t : *), t -> t" of
        Right t -> t `infersType` Star `inContext` []
  
  describe "typeC" $ do

    it "checks (t -> t) against type * in context [(t,*)]" $
      "t -> t" `checksType'` "*" `inContext` [("t", VStar)]

    it "checks (\\ a x . x) against type (forall (t : *), t -> t" $
      "(\\ a x . x)" `checksType'` "forall (t : *), t -> t"
        `inContext` []

    it "checks (forall (t : *), t -> t) against type *" $
      "forall (t : *), t -> t" `checksType'` "*" `inContext` []

    it "checks (fun x . x) against type (t -> t) in context [(t,*)]" $
      Lam (Inf (Bound 0)) `checksType` Pi (var' "t") (var' "t")
        `inContext` [("t", VStar)]

    it "checks (fun x y . x) against type (a -> b -> a)" $
      Lam (Lam (Inf (Bound 1)))
        `checksType` Pi (var' "a") (Inf (Pi (var' "b") (var' "a")))
        `inContext` [("a", VStar),("b", VStar)]

    it "checks (fun x y . y) against type (a -> b -> b)" $
      Lam (Lam (Inf (Bound 0)))
        `checksType` Pi (var' "a") (Inf (Pi (var' "b") (var' "b")))
        `inContext` [("a", VStar),("b", VStar)]

    it "checks (fun a x . x) against type (forall (t : *), t -> t)" $
      Lam (Lam (Inf (Bound 0)))
        `checksType` Pi (Inf Star) (Inf (Pi (Inf (Bound 0)) (Inf (Bound 1))))
        `inContext` []
    
    it ("check (((fun a x . x) : forall (t : *), t -> t) nat) " ++
      "against type (nat -> nat)") $
        "((fun a x . x) : forall (t : *), t -> t) nat"
          `checksType'` "nat -> nat"
          `inContext` [("nat", VStar)]