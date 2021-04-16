{-# LANGUAGE LambdaCase #-}

module GDTLC.Interpreter where

import GDTLC.Data.Term
import GDTLC.Eval
import GDTLC.Parser (parseProgram, parseTerm)
import System.IO
import GDTLC.Printer
import GDTLC.Typecheck
import GDTLC.Data.Context
import GDTLC.Data.Program
import Relude (StateT)
import GDTLC.Data.Name

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
  | ParseError String
  | Evaluated Value Type
  | EnvUpdated String Type
  deriving (Eq, Show)


interpStmt :: Env -> Stmt -> (Env, StmtResult)
interpStmt env stmt =
  case stmt of
    Let x ex ->
      case typeI 0 (toContext env) ex of
        Left err -> (env, TypeError err)
        Right t  -> ((x, t, evalI ex []):env, EnvUpdated x t)
    Expr e ->
      case typeI 0 (toContext env) e of
        Left err -> (env, TypeError err)
        Right t  -> (env, Evaluated (evalI e []) t)

interpExpr :: Env -> ITerm -> (Env, StmtResult)
interpExpr env e = interpStmt env (Expr e)

interpExprString :: Env -> String -> (Env, StmtResult)
interpExprString env str =
  case parseTerm str of
    Left err -> (env, ParseError (show err))
    Right e  -> interpExpr env e

interpProgString :: Env -> String -> (Env, StmtResult)
interpProgString env str =
  case parseProgram str of
    Left  err   -> (env, ParseError (show err))
    Right prog -> interpProgram env prog

-- note: fails on empty program
interpProgram :: Env -> Program -> (Env, StmtResult)
interpProgram env (Program [stmt]) = interpStmt env stmt
interpProgram env (Program (stmt:stmts)) =
  let (env', _) = interpStmt env stmt in
  interpProgram env' (Program stmts)

interpFile :: FilePath -> IO ()
interpFile fp =
  do
    rawProg <- readFile fp
    putStrLn ""
    case parseProgram rawProg of
      Left err -> print err
      Right prog -> do
        putStrLn $ "interpreting file " ++ fp ++ "..."
        putStrLn ""
        interpLoop baseEnv prog
  where
    interpLoop :: Env -> Program -> IO ()
    interpLoop _ (Program []) =
      putStrLn "finished interpreting program"

    interpLoop env (Program (x:xs)) = do
      let (env', res) = interpStmt env x
      case res of
        TypeError err -> do
          print err
          putStrLn $ "in statement " ++ printStmt x
        Evaluated v t ->
          putStrLn $ printValue 0 v ++ " @ " ++ printValue 0 t
        EnvUpdated x t ->
          putStrLn $ "defined " ++ x ++ " @ " ++ printValue 0 t
      putStrLn ""
      interpLoop env' (Program xs)
