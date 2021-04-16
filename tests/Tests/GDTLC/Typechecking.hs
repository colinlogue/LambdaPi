module Tests.GDTLC.Typechecking where

import GDTLC.Data.Term
import GDTLC.Data.Name
import Test.Hspec
import GDTLC.Data.Context
import GDTLC.Typecheck
import GDTLC.Eval
import Data.Bifunctor (Bifunctor(first))
import GDTLC.Parser (parseTerm, parseCTerm)
import Tests.GDTLC.Shared
import Tests.Utils


inferType :: ITerm -> Result Type
inferType = typeI 0 baseContext

natElimType :: Value
natElimType = evalI
  (unsafeParseTerm $
    "forall (m : nat -> *), m zero -> "
    ++ "(forall (n : nat), m n -> m (succ n)) -> "
    ++ "forall (k : nat), m k") []

typecheckTests :: IO ()
typecheckTests = hspec $ do

  pure ()
  
  describe "typeI" $ do

    it "nat ~> *" $
      inferType Nat `shouldBe` Right VStar
    
    it "zero ~> nat" $
      inferType Zero `shouldBe` Right VNat
    
    it "succ zero ~> nat" $
      inferType (Succ (Inf Zero)) `shouldBe` Right VNat
    
    it "natElim" $
      inferType (unsafeParseTerm "natElim") `shouldBe` Right natElimType
    
  describe "parsed expressions typecheck :" $ do

    let parseAndInfer = inferType . unsafeParseTerm

    it "nat ~> *" $
      parseAndInfer "nat" `shouldBe` Right VStar
    
    it "zero ~> nat" $
      parseAndInfer "zero" `shouldBe` Right VNat
    
    it "succ zero ~> nat" $
      parseAndInfer "succ zero" `shouldBe` Right VNat


