module Tests.GDTLC.Parsing where

import Test.Hspec
import Text.Parsec (runParser)
import GDTLC.Parser
import Data.Either (isLeft)
import Tests.GDTLC.Shared
import GDTLC.Data.Term
import GDTLC.Data.Program



parseTests :: IO ()
parseTests = hspec $ do

  describe "validIdent" $ do
    
    let parseIdent = runParser validIdent [] ""

    it "x is valid" $
      parseIdent "x" `shouldBe` Right "x"

    it "x1 is valid" $
      parseIdent "x1" `shouldBe` Right "x1"

    it "1x is not valid" $
      parseIdent "1x" `shouldSatisfy` isLeft

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
    
    it "parses fun notation" $
      parseTerm "(fun x . x) : t" `shouldBe`
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
        Right (Pi (Inf Star) (Inf (Pi (Inf (Bound 0)) (Inf (Bound 1)))))
    
    it "arrow associates to the right" $
      parseTerm "a -> b -> c" `shouldBe`
        Right (Pi (var' "a") (Inf (Pi (var' "b") (var' "c"))))
    
    it "parses compound lambda" $
      parseTerm "(\\ x y z . a) : t" `shouldBe`
        Right (Ann (Lam (Lam (Lam (var' "a")))) (var' "t"))
    
    it "binds terms correctly in nested lambda" $
      parseTerm "(\\ x . x (\\ y . x y (\\ z . x y z))) : t" `shouldBe`
        let lamZ = Lam (Inf ((Bound 2 :@: Inf (Bound 1)) :@: Inf (Bound 0))) in
        let lamY = Lam (Inf ((Bound 1 :@: Inf (Bound 0)) :@: lamZ)) in
        let lamX = Lam (Inf (Bound 0 :@: lamY)) in
          Right (Ann lamX (var' "t"))
    
    it "parses forall with two bindings" $
      parseTerm "forall ( x : a ) ( y : b ), t" `shouldBe`
        Right (Pi (var' "a") (Inf (Pi (var' "b") (var' "t"))))

    it "parses compound forall" $
      parseTerm "forall (x : t1) (y : t2) (z : t3), t4" `shouldBe`
        Right
          (Pi (var' "t1") (Inf
          (Pi (var' "t2") (Inf
          (Pi (var' "t3") (var' "t4"))))))

    it "binds terms correctly in nested pi" $
      parseTerm "forall (x : *) (y : x) (z : y), x" `shouldBe`
        let piZ = Pi (Inf (Bound 0)) (Inf (Bound 2)) in
        let piY = Pi (Inf (Bound 0)) (Inf piZ) in
        let piX = Pi (Inf Star) (Inf piY) in
          Right piX
  
    it "nat -> nat -> nat" $
      parseTerm "nat -> nat -> nat" `shouldBe`
        Right (Pi (Inf Nat) (Inf (Pi (Inf Nat) (Inf Nat))))
    
    it "simple forall in arrow expr" $
      parseTerm "nat -> (forall (x : nat), nat) -> nat"
        `shouldBe` Right
          (Pi
            (Inf Nat)
            (Inf (Pi
              (Inf (Pi
                (Inf Nat)
                (Inf Nat)))
              (Inf Nat))))

    it "forall in arrow expr" $
      parseTerm "nat -> (forall (x : nat), nat -> nat) -> nat"
        `shouldBe` Right
          (Pi
            (Inf Nat)
            (Inf (Pi
              (Inf (Pi
                (Inf Nat)
                (Inf (Pi
                  (Inf Nat)
                  (Inf Nat)))))
              (Inf Nat))))
    
    it "0 literal" $
      parseTerm "0" `shouldBe` Right Zero

    it "5 literal" $
      parseTerm "5" `shouldBe`
        Right (Succ (Inf (Succ (Inf (Succ (Inf (Succ (Inf (Succ (Inf Zero))))))))))

  describe "parseCTerm" $ do

    it "nat -> nat -> nat" $
      parseCTerm "nat -> nat -> nat" `shouldBe`
        Right (Inf (Pi (Inf Nat) (Inf (Pi (Inf Nat) (Inf Nat)))))

  describe "parseStmt" $ do

    it "parses def with explicit type" $
      parseStmt "def x : t := x end" `shouldBe`
        Right (Let "x" (Ann (var' "x") (var' "t")))
