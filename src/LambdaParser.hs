{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}

module LambdaParser where

import Control.Applicative (Alternative(..))
import Parser

newtype Var =
  Var String
  deriving (Show)

data Term
  = Variable Var
      -- | Defined DefinedTerm
  | Abstraction Var
                Term
  | Application Term
                Term
  deriving (Show)

term :: Parser Term
term = abstraction <|> application <|> var

varName :: Parser Var
varName = do
  sym <- alpha
  n <- many digit
  return $ Var (sym : n)

var :: Parser Term
var = Variable <$> varName

abstraction :: Parser Term
abstraction = do
  char '\\'
  v <- varName
  t <- term
  return $ Abstraction v t

application :: Parser Term
application = do
  t1 <- nestedIn "(" term ")"
  t2 <- term
  return $ Application t1 t2

lambdaTerm :: String -> Either String Term
lambdaTerm cs =
  case parse term cs of
    [(t, [])] -> Right t
    [(_, cs')] ->
      Left
        ("Cannot parse: " ++ take (length cs - length cs') cs ++ " ^ " ++ cs')
    _ -> Left ("Cannot parse " ++ cs)
