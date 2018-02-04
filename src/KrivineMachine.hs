module KrivineMachine (run) where

import LambdaCompiler (CTerm (..), ParseError, compiledTerm, convertCTermToString)

data Closure =
  Closure CTerm
          [[Closure]]
  deriving (Show)

krivine :: [Closure] -> Either (Int, Closure) [Closure]
krivine (Closure t e:s) =
  case t of
    Constant c -> Right $ Closure (Constant c) [] : s
    CVariable nu i ->
      case subs nu i e of
        Just cl -> Right $ cl : s
        Nothing -> Right $ Closure (CVariable nu i) [] : s
    CApplication v u -> Right $ Closure v e : Closure u e : s
    Lambda i v ->
      case pops i s of
        (0, xs') -> Right $ Closure v (pushs i 1 s : e) : xs'
        (k, _)   -> Left (k, Closure v (pushs i 1 s : e))

pops :: Int -> [a] -> (Int, [a])
pops 0 xs     = (0, xs)
pops i []     = (i, [])
pops i (_:xs) = pops (i - 1) xs

pushs :: Int -> Int -> [Closure] -> [Closure]
pushs 0 _ _      = []
pushs i k []     = Closure (CVariable 1 k) [] : pushs (i - 1) (k + 1) []
pushs i k (x:xs) = x : pushs (i - 1) k xs

subs :: Int -> Int -> [[Closure]] -> Maybe Closure
subs _ _ []      = Nothing
subs 1 i (x:_)   = sub i x
subs nu i (_:xs) = subs (nu - 1) i xs

sub :: Int -> [Closure] -> Maybe Closure
sub _ []     = Nothing
sub 1 (x:_)  = Just x
sub i (_:xs) = sub (i - 1) xs

close :: CTerm -> [Closure]
close t = [Closure t []]

initialState :: String -> Either ParseError [Closure]
initialState = fmap close . compiledTerm

krivineMachine :: [Closure] -> CTerm
krivineMachine xs =
  case krivine xs of
    Right (cl:xs') ->
      case cl of
        Closure (Constant c) _ -> apply (Constant c) xs'
        Closure (CVariable nu i) e ->
          case e of
            [] ->
              case xs' of
                [] -> CVariable nu i
                _  -> apply (CVariable nu i) xs'
            _ -> krivineMachine (cl:xs')
        Closure (Lambda _ _) _ -> krivineMachine (cl:xs')
        _ -> krivineMachine (cl:xs')
    Left (k, cl) -> Lambda k (krivineMachine [cl])

apply :: CTerm -> [Closure] -> CTerm
apply = foldl (\ t x -> CApplication t (krivineMachine [x]))

compute :: String -> Either ParseError String
compute = fmap (convertCTermToString . krivineMachine) . initialState

run :: IO String
run = (show . compute) <$> getLine

ex1 :: String
ex1 = "(\\xx)y"

ex2 :: String
ex2 = "(\\xy)(\\x(x)x)\\x(x)x"

ex3 :: String
ex3 = "\\f(\\x(f)(x)x)\\x(f)(x)x"
