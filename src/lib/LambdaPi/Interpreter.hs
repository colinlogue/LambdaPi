module LambdaPi.Interpreter where

import LambdaPi.Data.Term
import LambdaPi.Eval
import LambdaPi.Parser (parseProgram, parseTerm)
import System.IO
import LambdaPi.Printer
import LambdaPi.Typecheck
import LambdaPi.Data.Context
import LambdaPi.Data.Program
import Relude (StateT)
import LambdaPi.Data.Name

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


data StmtResult
  = TypeError [String]
  | Evaluated Value Type
  | EnvUpdated

type Env = [(String, Type, Value)]

toContext :: Env -> Context
toContext = fmap (\(x,t,_) -> (Global x,t))

interpStmt :: Env -> Stmt -> (Env, StmtResult)
interpStmt env stmt =
  case stmt of
    Let x ex ->
      case typeI 0 (toContext env) ex of
        Left err -> (env, TypeError err)
        Right t  -> ((x, t, evalI ex []):env, EnvUpdated)
    Expr e ->
      case typeI 0 (toContext env) e of
        Left err -> (env, TypeError err)
        Right t  -> (env, Evaluated (evalI e []) t)



interpFile :: FilePath -> IO ()
interpFile fp =
  do
    rawProg <- readFile fp
    case parseProgram rawProg of
      Left err -> print err
      Right prog ->
        interpLoop [] prog
  where
    interpLoop :: Env -> Program -> IO ()
    interpLoop _ (Program []) =
      putStrLn "finished interpreting program"
    interpLoop env (Program (x:xs)) = do
      let (env', res) = interpStmt env x
      case res of
        TypeError err ->
          print (err, x)
        Evaluated v t ->
          putStrLn $ printValue 0 v ++ " @ " ++ printValue 0 t
        EnvUpdated ->
          pure ()
      interpLoop env' (Program xs)
