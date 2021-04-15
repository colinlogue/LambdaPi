module Main where

import LambdaPi.Tests.Parsing
import LambdaPi.Tests.Typechecking
import LambdaPi.Tests.Evaluation

main :: IO ()
main = do
  parseTests
  evalTests
  typecheckingTests