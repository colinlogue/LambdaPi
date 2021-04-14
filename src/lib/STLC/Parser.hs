{-# LANGUAGE LambdaCase #-}

module STLC.Parser where

import STLC.Data.Program
import STLC.Data.Term

import Text.Parsec hiding (parse)
import STLC.Data.Name
import STLC.Data.Type
import Data.List (elemIndex)

type Bindings = [String]
type Parser = Parsec String Bindings

parse :: Parser a -> String -> Either ParseError a
parse p = runParser (p <* eof) [] ""

bindName :: Bindings -> String -> ITerm
bindName env x =
   case elemIndex x env of
     Just i  -> Bound i
     Nothing -> Free (Global x)


validIdent :: Parser String
validIdent = many1 alphaNum

ident :: Parser Ident
ident = Ident <$> validIdent

name :: Parser Name
name = Global <$> validIdent

withParens :: Parser a -> Parser a
withParens = between (char '(' <* spaces) (spaces <* char ')')

-- tries a parser but instead of failing returns nothing
tryMaybe :: Parser a -> Parser (Maybe a)
tryMaybe p = try (Just <$> p) <|> pure Nothing

--------------
-- PROGRAMS --
--------------

binding :: Parser Stmt
binding =
  Let
    <$  string "let" <* spaces
    <*> ident
    <*  spaces <* string "=" <* spaces
    <*> termExpr

-----------
-- TYPES --
-----------

typeExpr :: Parser Type
typeExpr =
  typeHead >>= \t1 ->
  tryMaybe typeTail >>= \case
    Just t2 -> pure $ Fun t1 t2
    Nothing -> pure t1

typeHead :: Parser Type
typeHead =
  withParens typeExpr
  <|> TFree <$> name

typeTail :: Parser Type
typeTail = spaces >> string "->" >> spaces >> typeExpr

parseType :: String -> Either ParseError Type
parseType = parse typeExpr


-----------
-- TERMS --
-----------

termExpr :: Parser ITerm
termExpr =
  termHead >>= \e1 -> spaces >> termTail e1
  <?> "term"

termHead :: Parser ITerm
termHead =
  try (withParens termExpr)
  <|>
  try (termIdent >>= \x ->
  tryMaybe annotation >>= \case
    Just t  -> pure $ Ann (Inf x) t
    Nothing -> pure x)
  <|>
  try (Ann <$> lambda <*> annotation)

termTail :: ITerm -> Parser ITerm
termTail e1 =
  (try ctermHead >>= \e2 -> spaces >> termTail (e1 :@: e2))
  <|>
  pure e1

termIdent :: Parser ITerm
termIdent =
  getState >>= \env ->
  bindName env <$> many1 letter

annotation :: Parser Type
annotation =
  spaces >> string ":" >> spaces >> typeExpr

lambda :: Parser CTerm
lambda =
  withParens $ do
    string "\\"
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