module LambdaParser where

import Control.Applicative (Alternative (..))
import Parser

type Var = String

data Term
  = Variable Var
  | Abstraction Var
                Term
  | Application Strategy Term
                Term
  deriving (Show)

data Strategy = Par | Seq deriving (Show)

parTerm :: Parser Term
parTerm = base <|> pTerm where
  base :: Parser Term
  base = do
    v <- nestedIn "(" var ")"
    t <- term
    return $ Application Par v t

  pTerm :: Parser Term
  pTerm = do
    t1 <- nestedIn "(" (base <|> pTerm) ")"
    t2 <- term
    return $ Application Par t1 t2

term :: Parser Term
term = parTerm <|> seqTerm

seqTerm :: Parser Term
seqTerm = abstraction <|> application <|> var

varName :: Parser Var
varName = do
  sym <- alpha
  n <- many digit
  return (sym : n)

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
  return $ Application Seq t1 t2

newtype ParseError = ParseError String deriving (Show)

parseLambdaTerm :: String -> Either ParseError Term
parseLambdaTerm cs =
  case parse term cs of
    [(t, [])] -> Right t
    [(_, cs')] ->
      Left $ ParseError
        ("Cannot parse: " ++ take (length cs - length cs') cs ++ " ^ " ++ cs')
    _ -> Left $ ParseError ("Cannot parse " ++ cs)
