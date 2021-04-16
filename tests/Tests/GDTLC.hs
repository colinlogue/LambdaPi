module Tests.GDTLC where

import Tests.GDTLC.Typechecking
import Tests.GDTLC.Prelude
import Tests.GDTLC.Parsing

runAll :: IO ()
runAll = do
  parseTests
  typecheckTests
  preludeTests