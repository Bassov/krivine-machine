module KrivineMachine (run) where

import Data.Either (either)
import LambdaCompiler (CTerm (..), ParseError, Strategy (..), compiledTerm)

data Closure =
  Closure CTerm Env
  deriving (Show)

type Stack = [Closure]
type Env = [[Closure]]

closure :: CTerm -> Closure
closure t = Closure t []

krivine :: Stack -> Either (Int, Closure) Stack
krivine (Closure t e:s) =
  case t of
    Constant c -> Right $ closure (Constant c) : s
    CVariable nu i ->
      case subs nu i e of
        Just cl -> Right $ cl : s
        Nothing -> Right $ closure (CVariable nu i) : s
    CApplication _ v u -> Right $ Closure v e : Closure u e : s
    Lambda i v ->
      case pops i s of
        (0, xs') -> Right $ Closure v (pushs i 1 s : e) : xs'
        (k, _)   -> Left (k, Closure v (pushs i 1 s : e))
  where
    pops 0 xs     = (0, xs)
    pops i []     = (i, [])
    pops i (_:xs) = pops (i - 1) xs

    pushs 0 _ _      = []
    pushs i k []     = closure (CVariable 1 k) : pushs (i - 1) (k + 1) []
    pushs i k (x:xs) = x : pushs (i - 1) k xs

    subs _ _ []      = Nothing
    subs 1 i (x:_)   = sub i x
    subs nu i (_:xs) = subs (nu - 1) i xs

    sub _ []     = Nothing
    sub 1 (x:_)  = Just x
    sub i (_:xs) = sub (i - 1) xs

initialState :: String -> Either ParseError Stack
initialState = fmap close . compiledTerm where
  close t = [closure t]

krivineMachine :: Stack -> CTerm
krivineMachine = either left right . krivine where
  left (k, cl) = Lambda k (krivineMachine [cl])

  right (cl:xs) =
    case cl of
      Closure (Constant c) _ -> apply (Constant c) xs
      Closure (CVariable nu i) e ->
        case e of
          [] ->
            case xs of
              [] -> CVariable nu i
              _  -> apply (CVariable nu i) xs
          _ -> krivineMachine (cl:xs)
      _ -> krivineMachine (cl:xs)

  apply = foldr (\x t -> CApplication Seq t (krivineMachine [x]))

compute :: String -> Either ParseError CTerm
compute = fmap krivineMachine . initialState

run :: IO String
run = (show . compute) <$> getLine

ex1 :: String
ex1 = "(\\xx)y"

ex2 :: String
ex2 = "(\\xy)(\\x(x)x)\\x(x)x"

ex3 :: String
ex3 = "\\f(\\x(f)(x)x)\\x(f)(x)x"
