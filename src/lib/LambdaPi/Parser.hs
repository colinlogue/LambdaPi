{-# LANGUAGE LambdaCase #-}

module LambdaPi.Parser where

import LambdaPi.Data.Term
import Text.Parsec hiding (parse)
import LambdaPi.Data.Name
import LambdaPi.Data.Env
import LambdaPi.Data.Context
import Data.List (elemIndex)
import LambdaPi.Eval
import Data.Functor (($>))
import LambdaPi.Data.Program

type Bindings = [String]
type Parser = Parsec String Bindings

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
  ]

termExpr :: Parser ITerm
termExpr =
  termHead >>= \e1 -> spaces >> termTail e1
  <?> "term"

termHead :: Parser ITerm
termHead =
  tryEach
    [ withParens termExpr
    , forallExpr
    , Ann <$> (Inf <$> termIdent) <*> annotation
    , Ann <$> lambda <*> annotation
    , Pi <$> (Inf <$> termIdent) <*> arrow
    , termIdent
    ]

termTail :: ITerm -> Parser ITerm
termTail e1 =
  try (ctermHead >>= \e2 -> spaces >> termTail (e1 :@: e2))
  <|>
  pure e1

termIdent :: Parser ITerm
termIdent =
  tryEach
    [ char '*' $> Star
    , getState >>= \env -> bindName env <$> validIdent
    ]

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

annotation :: Parser CTerm
annotation =
  spaces >> string ":" >> spaces >> cterm

arrow :: Parser CTerm
arrow =
  spaces >> string "->" >> spaces >> modifyState ("_":) >> cterm

lambda :: Parser CTerm
lambda =
  withParens $ do
    tryEach [ string "\\", string "fun" ]
    spaces
    xs <- reverse <$> many1 (validIdent <* spaces)
    env <- getState
    putState (xs ++ env)
    spaces
    string "."
    spaces
    body <- cterm
    putState env
    pure $ foldl (\x _ -> Lam x) body xs

cterm :: Parser CTerm
cterm = try lambda <|> Inf <$> termExpr

ctermHead :: Parser CTerm
ctermHead = try lambda <|> Inf <$> termHead

parseTerm :: String -> Either ParseError ITerm
parseTerm = parse termExpr

parseCTerm :: String -> Either ParseError CTerm
parseCTerm = parse cterm

--------------
-- Programs --
--------------

comment :: Parser ()
comment = string "--" >> manyTill anyChar endOfLine >> pure ()

letExpr :: Parser Stmt
letExpr =
  Let
    <$ string "def" <* spaces
    <*> validIdent <* spaces <* string ":=" <* spaces
    <*> termExpr <* spaces <* string "end"

stmt :: Parser Stmt
stmt =
  tryEach
    [ letExpr
    , Expr <$> termExpr
    ]

spacesAndComments :: Parser ()
spacesAndComments = spaces >> skipMany (comment >> spaces)

program :: Parser Program
program =
  Program <$ spacesAndComments <*> many (stmt <* spacesAndComments)

parseProgram :: String -> Either ParseError Program
parseProgram = parse program