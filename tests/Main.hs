module Main where

import LambdaPi.Tests.Parsing
import LambdaPi.Tests.Typechecking

main :: IO ()
main = do
  parseTests
  typecheckingTests