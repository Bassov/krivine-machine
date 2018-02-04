module LambdaCompiler (CTerm (..), compiledTerm, ParseError) where

import LambdaParser (ParseError, Term (..), Var, parseLambdaTerm)

data CTerm
  = CVariable Int
              Int
  | Constant Var
  | CApplication CTerm
                 CTerm
  | Lambda Int
           CTerm

instance Show CTerm where
  show t = showCTerm t []

normal :: CTerm -> CTerm
normal (CApplication v u) = CApplication (normal v) (normal u)
normal (Lambda 0 u) = normal u
normal (Lambda i u) =
  case normal u of
    Lambda i' u' -> Lambda (i + i') u'
    t            -> Lambda i t
normal t = t

translateAux :: Term -> [[Var]] -> Int -> Int -> Bool -> CTerm
translateAux t s nu i b =
  case t of
    Variable l ->
      case s of
        [] -> Constant l
        (l':c):s' ->
          if l == l'
            then CVariable (nu + 1) (length c + 1)
            else translateAux t (c : s') nu (i + 1) False
        []:s' -> translateAux t s' (nu + 1) 0 False
    Application v u ->
      CApplication (translateAux v s 0 0 False) (translateAux u s 0 0 False)
    Abstraction l u ->
      if b
        then case s of
               c:s' -> Lambda 1 (translateAux u ((l : c) : s') 0 0 True)
        else Lambda 1 (translateAux u ([l] : s) 0 0 True)

compiledTerm :: String -> Either ParseError CTerm
compiledTerm = fmap compile . parseLambdaTerm where
  compile t = normal (translateAux t [] 0 0 False)

showCTerm :: CTerm -> [Int] -> String
showCTerm t l =
  case t of
    CVariable nu i -> showVariable nu i
    Constant c -> show c
    CApplication v u ->
      "(" ++ showCTerm v l ++ ")" ++ showCTerm u l
    Lambda i v -> showLambda i ++ showCTerm v (i : l)
  where
    showVariable nu i  = 'x' : show (i + sum (drop nu l))

    showLambda 0 = ""
    showLambda i = showLambda (i - 1) ++ '\\' : 'x' : show (sum l + i)
