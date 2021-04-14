module LambdaPi.Printer where

import LambdaPi.Data.Term
import LambdaPi.Data.Name
import LambdaPi.Eval (quote)



printTerm :: ITerm -> String
printTerm (Ann e t) = "(" ++ printCTerm e ++ " : " ++ printCTerm t ++ ")"
printTerm (Bound i) = show i
printTerm (Free x)  = printName x
printTerm (e1 :@: (Inf e2@(_ :@: _))) =
  printTerm e1 ++ " (" ++ printTerm e2 ++ ")"
printTerm (e1 :@: e2) = printTerm e1 ++ " " ++ printCTerm e2
printTerm Star = "*"
printTerm (Pi t t') = "(# " ++ printCTerm t ++ " . " ++ printCTerm t' ++ ")"

printCTerm :: CTerm -> String
printCTerm (Inf t) = printTerm t
printCTerm (Lam t) = "(\\." ++ printCTerm t ++ ")"

printName :: Name -> String
printName (Global x) = x
printName (Local i)  = "~" ++ show i
printName (Quote i)  = "`" ++ show i

printValue :: Int -> Value -> String
printValue i = printCTerm . quote i