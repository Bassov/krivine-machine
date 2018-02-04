{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Applicative (Alternative(..))
import Data.Bifunctor (first)
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Monoid ((<>))

newtype Parser a = Parser
  { parse :: String -> [(a, String)]
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \cs -> fmap (first f) (p cs)

instance Applicative Parser where
  pure a = Parser $ \cs -> [(a, cs)]
  Parser p1 <*> Parser p2 =
    Parser $ \s ->
      concat [concat [[(y w, cs')] | (w, cs') <- p2 cs] | (y, cs) <- p1 s]

instance Monad Parser where
  return = pure
  Parser p >>= f =
    Parser $ \str -> concat [parse (f a) str' | (a, str') <- p str]

instance Monoid (Parser a) where
  mempty = Parser $ const []
  mappend p q = Parser $ \str -> parse p str ++ parse q str

instance Alternative Parser where
  empty = mempty
  p <|> q =
    Parser $ \str ->
      case parse (p <> q) str of
        [] -> []
        x:_ -> [x]

item :: Parser Char
item =
  Parser $ \case
    "" -> []
    c:str' -> [(c, str')]

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c
    then return c
    else empty

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
  char c
  string cs
  return (c : cs)

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) <|> return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
  a <- p
  as <- many (sep >> p)
  return (a : as)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= \a -> rest a
  where
    rest a =
      flip (<|>) (return a) $ do
        f <- op
        a' <- p
        rest (f a a')

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do
  a <- p
  space
  return a

symb :: String -> Parser String
symb = token . string

apply :: Parser a -> String -> [(a, String)]
apply p = parse (space >> p)

nestedIn :: String -> Parser a -> String -> Parser a
nestedIn l p r = do
  symb l
  a <- p
  symb r
  return a

digit :: Parser Char
digit = token (sat isDigit)

alpha :: Parser Char
alpha = token (sat isAlpha)
