module Krivine.Sequential where

import Data.Either (either)
import Krivine.Core (CTerm (..), Closure (..), ParseError, Stack, initialState, krivine)

import Criterion.Main

ex1 :: String
ex1 = "(\\xx)y"

ex2 :: String
ex2 = "(((x)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)z)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)z)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)z"

runSequentialKrivine =
  defaultMain [
        bgroup "sequential" [ bench "1" $ whnf compute ex1
                          , bench "2" $ whnf compute ex2
                          ]
        ]

krivineMachine :: Stack -> CTerm
krivineMachine = either left right . krivine where
  left (k, cl) = Lambda k (krivineMachine [cl])

  right stack@(cl:xs) =
    case cl of
      Closure (Constant c) _ -> apply (Constant c) xs
      Closure (CVariable nu i) e ->
        case e of
          [] ->
            case xs of
              [] -> CVariable nu i
              _  -> apply (CVariable nu i) xs
          _ -> krivineMachine stack
      _ -> krivineMachine stack
  apply = foldl (\ t x -> CApplication t (krivineMachine [x]))

compute :: String -> Either ParseError CTerm
compute = fmap krivineMachine . initialState
