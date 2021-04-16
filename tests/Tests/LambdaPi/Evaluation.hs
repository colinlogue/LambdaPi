module Tests.LambdaPi.Evaluation where

import LambdaPi.Data.Context
import LambdaPi.Data.Term
import LambdaPi.Eval
import Test.Hspec
import LambdaPi.Data.Name


var :: String -> ITerm
var x = Free (Global x)

var' :: String -> CTerm
var' = Inf . var

varV :: String -> Value
varV = VNeutral . NFree . Global


evalTests :: IO ()
evalTests = hspec $ do

  describe "evalI" $ do

    it "free var ~> neutral" $
      evalI (var "x") [] `shouldBe` varV "x"
    
    it "* ~> *" $
      evalI Star [] `shouldBe` VStar
    
    it "bound var ~> value in env" $
      evalI (Bound 0) [varV "val"] `shouldBe` varV "val"

    it "id x ~> x" $
      evalI (Bound 0 :@: var' "x") [VLam id] `shouldBe` varV "x"
    
    it "((fun x . x) : t) y ~> y" $
      evalI (Ann (Lam (Inf (Bound 0))) (var' "t") :@: var' "y") []
        `shouldBe` varV "y"