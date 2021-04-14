module STLC.Printer where

import STLC.Data.Term
import STLC.Data.Type
import STLC.Data.Name
import STLC.Data.Value
import STLC.Eval (quote)



printTerm :: ITerm -> String
printTerm (Ann e t) = "(" ++ printCTerm e ++ " : " ++ printType t ++ ")"
printTerm (Bound i) = show i
printTerm (Free x)  = printName x
printTerm (e1 :@: (Inf e2@(_ :@: _))) =
  printTerm e1 ++ " (" ++ printTerm e2 ++ ")"
printTerm (e1 :@: e2) = printTerm e1 ++ " " ++ printCTerm e2

printCTerm ::CTerm -> String
printCTerm (Inf t) = printTerm t
printCTerm (Lam t) = "(\\." ++ printCTerm t ++ ")"

printType :: Type -> String
printType (TFree x) = printName x
printType (Fun (TFree x) t2) = printName x ++ " -> " ++ printType t2
printType (Fun t1 t2) =
  "(" ++ printType t1 ++ " -> " ++ printType t2 ++ ")"

printName :: Name -> String
printName (Global x) = x
printName (Local i)  = "~" ++ show i
printName (Quote i)  = "`" ++ show i

printValue :: Int -> Value -> String
printValue i = printCTerm . quote i