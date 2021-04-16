module Tests.Utils where

import GDTLC.Data.Term
import GDTLC.Parser (parseTerm)


unsafeParseTerm :: String -> ITerm
unsafeParseTerm str =
  case parseTerm str of
    Right e -> e