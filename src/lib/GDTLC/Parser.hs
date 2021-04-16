{-# LANGUAGE LambdaCase #-}

module GDTLC.Parser where

import GDTLC.Data.Term
import Text.Parsec hiding (parse)
import GDTLC.Data.Name
import GDTLC.Data.Env
import GDTLC.Data.Context
import Data.List (elemIndex)
import GDTLC.Eval
import Data.Functor (($>))
import GDTLC.Data.Program
import Control.Monad (void)

type Bindings = [String]
type Parser = Parsec String Bindings

spaces1 :: Parser ()
spaces1 = void $ many1 space

parse :: Parser a -> String -> Either ParseError a
parse p = runParser (p <* spaces <* eof) [] ""

bindName :: Bindings -> String -> ITerm
bindName env x =
   case elemIndex x env of
     Just i  -> Bound i
     Nothing -> Free (Global x)

isKeyword :: String -> Bool
isKeyword x = elem x keywords

validIdent :: Parser String
validIdent = (:) <$> letter <*> many alphaNum >>= notKeyword
  where
    notKeyword :: String -> Parser String
    notKeyword x =
      if isKeyword x then
        fail $ x ++ " is a reserved keyword"
      else
        pure x

name :: Parser Name
name = Global <$> validIdent

withParens :: Parser a -> Parser a
withParens = between (char '(' <* spaces) (spaces <* char ')')

-- tries a parser but instead of failing returns nothing
tryMaybe :: Parser a -> Parser (Maybe a)
tryMaybe p = try (Just <$> p) <|> pure Nothing

tryEach :: [Parser a] -> Parser a
tryEach = choice . fmap try

-----------
-- TERMS --
-----------

keywords =
  [ "forall"
  , "fun"
  , "def"
  , "end"
  , "let"
  , "eval"
  ]

termExpr :: Parser ITerm
termExpr =
  termHead >>= \e1 -> spaces >> termTail e1
  <?> "term"

natElimType :: CTerm
natElimType = quote 0 $
  VPi mType
    (\m -> VPi (mzType m)
    (\_ -> VPi (msType m)
    (\_ -> VPi VNat
    (\k -> m `vapp` k))))
  where
    mType :: Type
    mType = VPi VNat (const VStar)

    mzType :: Type -> Type
    mzType m = m `vapp` VZero

    msType :: Type -> Type
    msType m = VPi VNat (\l -> VPi (m `vapp` l) (\_ -> m `vapp` VSucc l))

natElimTerm :: CTerm
natElimTerm =
  Lam (Lam (Lam (Lam body)))
    where
      body :: CTerm
      body = Inf (NatElim
        (Inf (Bound 3))
        (Inf (Bound 2))
        (Inf (Bound 1))
        (Inf (Bound 0)))

termHead :: Parser ITerm
termHead =
  tryEach
    [ withParens termExpr
    , forallExpr
    , natLiteral
    , Ann <$> lambda <*> annotation
    , termIdent
    ] <?> "term head"

termTail :: ITerm -> Parser ITerm
termTail e1 =
  tryEach
    [ ctermHead >>= \e2 -> spaces >> termTail (e1 :@: e2)
    , Ann (Inf e1) <$ string ":" <* spaces <*> cterm
    , Pi (Inf e1) <$ string "->" <* spaces <* modifyState ("_":) <*> cterm
    , pure e1
    ] <?> "term tail"

termIdent :: Parser ITerm
termIdent =
  tryEach
    [ char '*' $> Star
    , validIdent >>= \case
        "nat"     -> pure Nat
        "zero"    -> pure Zero
        "succ"    -> pure $ Ann (Lam (Inf (Succ (Inf (Bound 0))))) (Inf (Pi (Inf Nat) (Inf Nat)))
        "natElim" -> pure $ Ann natElimTerm natElimType
        _ -> fail "not a valid nat keyword"
    , getState >>= \env -> bindName env <$> validIdent
    ] <?> "identifier"

binder :: Parser CTerm
binder = do
  string "("
  spaces
  x <- validIdent
  spaces
  string ":"
  spaces
  t <- cterm
  -- x can't appear in t (which is the type of x)
  modifyState (x:)
  spaces
  string ")"
  pure t
  <?> "pi binder"

forallExpr :: Parser ITerm
forallExpr = do
  string "forall "
  spaces
  env <- getState
  xs <- reverse <$> many1 (binder <* spaces)
  string ","
  spaces
  body <- cterm
  putState env
  case foldl (\b x -> Inf $ Pi x b) body xs of
    Inf (Pi x t) -> pure (Pi x t)
    _ -> fail "no binding found for pi type"
  <?> "forall expr"

annotation :: Parser CTerm
annotation =
  spaces >> string ":" >> spaces >> cterm
  <?> "annotation"

-- arrowExpr :: Parser ITerm
-- arrowExpr = do
--   t1 <- termExpr
--   ts <- many1 arrow
--   case foldl (\b t -> Inf $ Pi t b) 

-- arrow :: Parser CTerm
-- arrow =
--   spaces >> string "->" >> spaces >> modifyState ("_":) >> cterm
--   <?> "arrow"

lambda :: Parser CTerm
lambda =
  withParens (do
    tryEach [ string "\\", string "fun" ]
    spaces
    xs <- reverse <$> many1 (tryEach [validIdent, string "_"] <* spaces)
    env <- getState
    putState (xs ++ env)
    spaces
    string "."
    spaces
    body <- cterm
    putState env
    pure $ foldl (\x _ -> Lam x) body xs)
    <?> "lambda"

natLiteral :: Parser ITerm
natLiteral = do
  raw_x <- many1 digit
  let x = read raw_x
  pure (toNat x)
    where
      toNat :: Int -> ITerm
      toNat 0 = Zero
      toNat x = Succ (Inf (toNat (x-1)))

cterm :: Parser CTerm
cterm = try lambda <|> Inf <$> termExpr <?> "cterm"

ctermHead :: Parser CTerm
ctermHead = try lambda <|> Inf <$> termHead <?> "cterm head"

parseTerm :: String -> Either ParseError ITerm
parseTerm = parse termExpr

parseCTerm :: String -> Either ParseError CTerm
parseCTerm = parse cterm

--------------
-- Programs --
--------------

comment :: Parser ()
comment = string "--" >> manyTill anyChar endOfLine >> pure ()

defExpr :: Parser Stmt
defExpr =
  tryEach
    [ (\x t b -> Let x (Ann b t))
      <$ string "def" <* spaces
      <*> validIdent <* spaces <* string ":" <* spaces
      <*> cterm <* spaces <* string ":=" <* spaces
      <*> cterm <* spaces <* string "end"
    , Let
      <$ string "def" <* spaces
      <*> validIdent <* spaces <* string ":=" <* spaces
      <*> termExpr <* spaces <* string "end"
    ]

evalExpr :: Parser Stmt
evalExpr =
  Expr
    <$ string "eval" <* spaces
    <*> termExpr <* spaces <* string "end"

stmt :: Parser Stmt
stmt =
  tryEach
    [ defExpr
    , evalExpr
    ]

includeStmt :: Parser Stmt
includeStmt =
  Include
    <$ string "include" <* spaces1
    <*> validIdent <* endOfLine

spacesAndComments :: Parser ()
spacesAndComments = spaces >> skipMany (comment >> spaces)

program :: Parser Program
program =
  Program <$ spacesAndComments <*> many (stmt <* spacesAndComments)

parseStmt :: String -> Either ParseError Stmt
parseStmt = parse stmt

parseProgram :: String -> Either ParseError Program
parseProgram = parse program