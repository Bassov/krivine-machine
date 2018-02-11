{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Krivine.Core
       ( CTerm (..)
       , Stack
       , Env
       , Closure (..)
       , ParseError
       , krivine
       , initialState
       ) where

import Data.Binary
import Data.Typeable
import GHC.Generics
import LambdaCompiler (CTerm (..), ParseError, compiledTerm)

data Closure =
  Closure CTerm Env
  deriving (Show, Generic, Typeable)

instance Binary Closure

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
    CApplication v u -> Right $ Closure v e : Closure u e : s
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
