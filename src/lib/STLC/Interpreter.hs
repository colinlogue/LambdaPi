module STLC.Interpreter where

import STLC.Data.Term
import STLC.Eval
import STLC.Parser (parseTerm)
import System.IO
import STLC.Printer

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
        putStrLn $ "evaluating " ++ printTerm term ++ "..."
        let v = evalI term []
        putStrLn (printValue 0 v)
        repl
