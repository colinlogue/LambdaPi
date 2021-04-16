module GDTLC.Printer where

import GDTLC.Data.Term
import GDTLC.Data.Name
import GDTLC.Eval (quote)
import GDTLC.Data.Program



printTerm :: ITerm -> String
printTerm (Ann e t) = "(" ++ printCTerm e ++ " : " ++ printCTerm t ++ ")"
printTerm (Bound i) = "<" ++ show i ++ ">"
printTerm (Free x)  = printName x
printTerm (e1 :@: (Inf e2@(_ :@: _))) =
  printTerm e1 ++ " (" ++ printTerm e2 ++ ")"
printTerm (e1 :@: e2) = printTerm e1 ++ " " ++ printCTerm e2
printTerm Star = "*"
printTerm (Pi t t') = "(# " ++ printCTerm t ++ " . " ++ printCTerm t' ++ ")"
-- Nat
printTerm Nat = "Nat"
printTerm Zero = "0"
printTerm (Succ k) = printSucc k
printTerm (NatElim m mz ms k) =
  "natElim ("
  ++ printCTerm m  ++ ") ("
  ++ printCTerm mz ++ ") ("
  ++ printCTerm ms ++ ") ("
  ++ printCTerm k  ++ ")"

printSucc :: CTerm -> String
printSucc k =
  case toNum k of
    Just x -> show (1 + x)
    Nothing -> "succ (" ++ printCTerm k ++ ")"
  where
    toNum :: CTerm -> Maybe Int
    toNum (Inf Zero) = Just 0
    toNum (Inf (Succ x)) = (1+) <$> toNum x

printCTerm :: CTerm -> String
printCTerm (Inf t) = printTerm t
printCTerm (Lam t) = "(\\." ++ printCTerm t ++ ")"

printName :: Name -> String
printName (Global x) = x
printName (Local i)  = "~" ++ show i
printName (Quote i)  = "`" ++ show i

printValue :: Int -> Value -> String
printValue i = printCTerm . quote i

printStmt :: Stmt -> String
printStmt (Let x e) = "let " ++ x ++ " := " ++ printTerm e
printStmt (Expr e) = printTerm e