module Tests.GDTLC.Prelude where

import Test.Hspec
import Tests.GDTLC.Shared
import GDTLC.Interpreter
import GDTLC.Typecheck
import GDTLC.Data.Term
import GDTLC.Data.Context
import GDTLC.Eval

inferType :: Env -> Value -> Result Type
inferType env v =
  case quote 0 v of
    Inf e -> typeI 0 (toContext env) e
    Lam _ -> Left ["can't infer type for lambda"]


preludeTests :: IO ()
preludeTests =
  
  readFile "data/prelude.lp" >>= \raw_prelude -> hspec $ do

    let (env,_) = interpProgString baseEnv raw_prelude

    describe "function types :" $ do

      it "id nat zero  @  nat" $
        let (_, Evaluated v t) = interpExprString env "id nat zero" in
        let Right t = inferType env v in
        t `shouldBe` VNat