module Main where

import qualified Tests.LambdaPi
import qualified Tests.GDTLC

main :: IO ()
main = do
  Tests.LambdaPi.runAll
  Tests.GDTLC.runAll