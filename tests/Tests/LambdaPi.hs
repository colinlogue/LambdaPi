module Tests.LambdaPi where

import Tests.LambdaPi.Parsing (parseTests)
import Tests.LambdaPi.Evaluation (evalTests)
import Tests.LambdaPi.Typechecking (typecheckingTests)


runAll :: IO ()
runAll = do
  parseTests
  evalTests
  typecheckingTests