module LambdaCompiler where

import LambdaParser

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
        Constant (Var l) -> Lambda i (Constant (Var l))
        CVariable i' n -> Lambda i (CVariable i' n)
        CApplication v u' -> Lambda i (CApplication v u')
        Lambda i' u' -> Lambda (i + i') u'
    _ -> t

replace :: CTerm -> String -> Int -> Int -> CTerm
replace t l nu i =
  case t of
    CVariable nu i -> CVariable nu i
    Constant (Var l') ->
      if l == l'
        then CVariable nu i
        else Constant (Var l')
    CApplication v u -> CApplication (replace v l nu i) (replace u l nu i)
    Lambda j u -> Lambda j (replace u l (nu + 1) i)

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

compiledTerm :: String -> Either String CTerm
compiledTerm cs =
  case lambdaTerm cs of
    Left cs' -> Left cs'
    Right t -> Right (compile t)

substitute :: CTerm -> Var -> CTerm -> CTerm
substitute t (Var l) u =
  case t of
    CVariable _ _ -> t
    Constant (Var c) ->
      if l == c
        then u
        else t
    CApplication u1 u2 ->
      CApplication (substitute u1 (Var l) u) (substitute u2 (Var l) u)
    Lambda i v -> Lambda i (substitute v (Var l) u)

substituteTerm :: Term -> Var -> Term -> CTerm
substituteTerm t x u = substitute (compile t) x (compile u)

substitutelambda :: String -> Var -> String -> Either String CTerm
substitutelambda cs v cs' =
  case lambdaTerm cs of
    Left l -> Left l
    Right r -> fmap (substituteTerm r v) (lambdaTerm cs')

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

substitution :: String -> String -> String -> String
substitution cs v cs' =
  case substitutelambda cs (Var v) cs' of
    Left l -> l
    Right t -> convertCTermToString t

convertCTermToString :: CTerm -> String
convertCTermToString t = convertCTermToStringaux t []
