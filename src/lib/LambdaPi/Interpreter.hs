module LambdaPi.Interpreter where

import LambdaPi.Data.Term
import LambdaPi.Eval
import LambdaPi.Parser (parseTerm)
import System.IO
import LambdaPi.Printer
import LambdaPi.Typecheck

flush :: IO ()
flush = hFlush stdout

prompt :: String
prompt = "> "

showPrompt :: IO ()
showPrompt = putStr prompt >> flush

repl :: IO ()
repl =
  do
    showPrompt
    line <- getLine
    case parseTerm line of
      Left err -> do print err; repl
      Right term -> do
        putStrLn $ "parsed term: " ++ printTerm term
        case typeI 0 [] term of
          Left err -> do
            putStrLn $ "could not infer type"
            print err
            repl
          Right t -> do
            putStrLn $ "type: " ++ printValue 0 t
            let v = evalI term []
            putStrLn $ "value: " ++ printValue 0 v
            repl
