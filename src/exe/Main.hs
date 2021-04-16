{-# LANGUAGE LambdaCase #-}

module Main where

import GDTLC.Interpreter
import System.Environment

main :: IO ()
main =
  getArgs >>= \case
    [] -> putStrLn "no filepath given"
    path:_ -> interpFile path