module KrivineMachine where

import LambdaCompiler

data Stack a
  = EmptyStack
  | Push a
         (Stack a)
  deriving (Show)

push :: a -> Stack a -> Stack a
push = Push

top :: Stack a -> a
top (Push x _) = x

pop :: Stack a -> Stack a
pop (Push _ xs) = xs

emptyStack :: Stack a
emptyStack = EmptyStack

isEmpty :: Stack a -> Bool
isEmpty xs =
  case xs of
    EmptyStack -> True
    _ -> False

data Closure =
  Closure CTerm
          (Stack (Stack Closure))
  deriving (Show)

data None =
  None

krivine :: Stack Closure -> Either (Int, Closure) (Stack Closure)
krivine (Push (Closure t e) s) =
  case t of
    Constant c -> Right (Push (Closure (Constant c) EmptyStack) s)
    CVariable nu i ->
      case subs nu i e of
        Right cl -> Right (Push cl s)
        Left None -> Right (Push (Closure (CVariable nu i) EmptyStack) s)
    CApplication v u -> Right (Push (Closure v e) (Push (Closure u e) s))
    Lambda i v ->
      case pops i s of
        (0, xs') -> Right (Push (Closure v (Push (pushs i 1 s) e)) xs')
        (k, _) -> Left (k, Closure v (Push (pushs i 1 s) e))

pops :: Int -> Stack a -> (Int, Stack a)
pops i xs =
  case i of
    0 -> (0, xs)
    _ ->
      case xs of
        Push _ xs' -> pops (i - 1) xs'
        _ -> (i, EmptyStack)

pushs :: (Num a, Eq a) => a -> Int -> Stack Closure -> Stack Closure
pushs i k xs =
  case i of
    0 -> EmptyStack
    _ ->
      case xs of
        Push x xs' -> Push x (pushs (i - 1) k xs')
        _ ->
          Push
            (Closure (CVariable 1 k) EmptyStack)
            (pushs (i - 1) (k + 1) EmptyStack)

subs :: Int -> Int -> Stack (Stack Closure) -> Either None Closure
subs nu i xs =
  case nu of
    1 ->
      case xs of
        Push s _ -> sub i s
        _ -> Left None
    _ ->
      case xs of
        Push _ xs' -> subs (nu - 1) i xs'
        _ -> Left None

sub :: Int -> Stack Closure -> Either None Closure
sub i xs =
  case i of
    1 ->
      case xs of
        Push c _ -> Right c
        _ -> Left None
    _ ->
      case xs of
        Push _ xs' -> sub (i - 1) xs'
        _ -> Left None

completeEnvironment :: Int -> Stack Closure -> Stack Closure
completeEnvironment j xs =
  case j of
    0 -> xs
    _ ->
      Push (Closure (CVariable 1 j) EmptyStack) (completeEnvironment (j - 1) xs)

close :: CTerm -> Stack Closure
close t = Push (Closure t EmptyStack) EmptyStack

currentTerm :: Stack Closure -> CTerm
currentTerm xs =
  case xs of
    Push c _ ->
      case c of
        Closure t _ -> t

state :: String -> Either String (Stack Closure)
state s =
  case compiledTerm s of
    Right t -> Right (close t)
    Left cs -> Left cs

environment :: Stack Closure -> Stack (Stack Closure)
environment xs =
  case xs of
    Push c _ ->
      case c of
        Closure _ e -> e

krivineMachine :: Stack Closure -> CTerm
krivineMachine xs =
  case krivine xs of
    Right (Push cl xs') ->
      case cl of
        Closure (Constant c) _ -> apply (Constant c) xs'
        Closure (CVariable nu i) e ->
          case e of
            EmptyStack ->
              case xs' of
                EmptyStack -> CVariable nu i
                _ -> apply (CVariable nu i) xs'
            _ -> krivineMachine (Push cl xs')
        Closure (Lambda _ _) _ -> krivineMachine (Push cl xs')
        _ -> krivineMachine (Push cl xs')
    Left (k, cl) -> Lambda k (krivineMachine (Push cl EmptyStack))

apply :: CTerm -> Stack Closure -> CTerm
apply t xs =
  case xs of
    EmptyStack -> t
    Push c xs' ->
      apply (CApplication t (krivineMachine (Push c EmptyStack))) xs'

computeCTerm :: CTerm -> CTerm
computeCTerm t = krivineMachine (close t)

compute :: String -> String
compute s =
  case compiledTerm s of
    Right _ ->
      case state s of
        Right t -> convertCTermToString (krivineMachine t)
    Left cs -> cs

environmentState :: Stack Closure -> Stack (Stack Closure)
environmentState s =
  case top s of
    Closure _ e -> e

run :: IO String
run = getLine >>= \x -> return (compute x)

ex1 :: String
ex1 = "(\\xx)y"

ex2 :: String
ex2 = "(\\xy)(\\x(x)x)\\x(x)x"

ex3 :: String
ex3 = "\\f(\\x(f)(x)x)\\x(f)(x)x"
