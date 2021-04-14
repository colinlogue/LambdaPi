module LambdaPi.Tests.Parsing where

import LambdaPi.Data.Name
import LambdaPi.Data.Term
import LambdaPi.Parser

import Test.Hspec

-- helpers for writing constructors more succinctly
var :: String -> ITerm
var x = Free (Global x)

var' :: String -> CTerm
var' x = Inf (var x)

parseTests :: IO ()
parseTests = hspec $ do
  describe "parseTerm" $ do

    it "parses free variable" $
      parseTerm "x" `shouldBe` Right (var "x")

    it "parses free variable in parens" $
      parseTerm "( x )" `shouldBe` Right (var "x")

    it "parses simple application" $
      parseTerm "x y" `shouldBe` Right (var "x" :@: var' "y")

    it "application associates to the left" $
      parseTerm "x y z" `shouldBe`
        Right ((var "x" :@: var' "y") :@: var' "z")
    
    it "parses simple annotation" $
      parseTerm "x : t" `shouldBe` Right (Ann (var' "x") (var' "t"))

    it "parses simple lambda" $
      parseTerm "(\\ x . x) : t" `shouldBe`
        Right (Ann (Lam (Inf (Bound 0))) (var' "t"))
    
    it "parses simple forall" $
      parseTerm "forall (x : t), t" `shouldBe`
        Right (Pi (var' "t") (var' "t"))

    it "parses simple arrow" $
      parseTerm "t -> t" `shouldBe`
        Right (Pi (var' "t") (var' "t"))
    
    it "parses star" $
      parseTerm "*" `shouldBe` Right Star

    it "parses: * -> *" $
      parseTerm "* -> *" `shouldBe` Right (Pi (Inf Star) (Inf Star))
    
    it "parses: forall (t : *), t -> t" $
      parseTerm "forall (t : *), t -> t" `shouldBe`
        Right (Pi (Inf Star) (Inf (Pi (Inf (Bound 0)) (Inf (Bound 0)))))
    
    it "arrow associates to the right" $
      parseTerm "a -> b -> c" `shouldBe`
        Right (Pi (var' "a") (Inf (Pi (var' "b") (var' "c"))))