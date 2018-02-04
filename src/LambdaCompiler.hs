module LambdaCompiler (CTerm (..), compiledTerm, convertCTermToString, ParseError) where

import LambdaParser (ParseError, Term (..), Var (..), parseLambdaTerm)

data CTerm
  = CVariable Int
              Int
  | Constant Var
  | CApplication CTerm
                 CTerm
  | Lambda Int
           CTerm
  deriving (Show)

normal :: CTerm -> CTerm
normal t =
  case t of
    CApplication v u -> CApplication (normal v) (normal u)
    Lambda 0 u -> normal u
    Lambda i u ->
      case normal u of
        Constant (Var l)  -> Lambda i (Constant (Var l))
        CVariable i' n    -> Lambda i (CVariable i' n)
        CApplication v u' -> Lambda i (CApplication v u')
        Lambda i' u'      -> Lambda (i + i') u'
    _ -> t

translateAux :: Term -> [[Var]] -> Int -> Int -> Bool -> CTerm
translateAux t s nu i b =
  case t of
    Variable (Var l) ->
      case s of
        [] -> Constant (Var l)
        (Var l':c):s' ->
          if l == l'
            then CVariable (nu + 1) (length c + 1)
            else translateAux t (c : s') nu (i + 1) False
        []:s' -> translateAux t s' (nu + 1) 0 False
    Application v u ->
      CApplication (translateAux v s 0 0 False) (translateAux u s 0 0 False)
    Abstraction (Var l) u ->
      if b
        then case s of
               c:s' -> Lambda 1 (translateAux u ((Var l : c) : s') 0 0 True)
        else Lambda 1 (translateAux u ([Var l] : s) 0 0 True)

compile :: Term -> CTerm
compile t = normal (translateAux t [] 0 0 False)

compiledTerm :: String -> Either ParseError CTerm
compiledTerm = fmap compile . parseLambdaTerm

printvariable :: [Int] -> Int -> Int -> String
printvariable l nu i =
  case nu of
    0 -> "x_" ++ show (i + sum l)
    _ ->
      case l of
        _:l -> printvariable l (nu - 1) i

convertCTermToStringaux :: CTerm -> [Int] -> String
convertCTermToStringaux t l =
  case t of
    CVariable nu i -> printvariable l nu i
    Constant (Var c) -> c
    CApplication v u ->
      "(" ++ convertCTermToStringaux v l ++ ")" ++ convertCTermToStringaux u l
    Lambda i v -> printlambda l i ++ convertCTermToStringaux v (i : l)

printlambda :: [Int] -> Int -> String
printlambda l i =
  case i of
    0 -> ""
    _ -> printlambda l (i - 1) ++ "\\" ++ "x_" ++ show (sum l + i)

convertCTermToString :: CTerm -> String
convertCTermToString t = convertCTermToStringaux t []
