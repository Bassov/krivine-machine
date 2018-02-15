{-# LANGUAGE BangPatterns #-}

module Krivine.Sequential where

import Data.Either (either)
import Krivine.Core (CTerm (..), Closure (..), ParseError, Stack, initialState, krivine)

import Criterion.Main

import Data.List
import Data.List.Index
import Data.Time

-- runSequentialKrivine term =
--   defaultMain [
--         bgroup "sequential" [ bench "1" $ whnf compute term
--                           ]
--         ]

runSequentialKrivine :: [Stack] -> IO ()
runSequentialKrivine = imapM_ calculate
  where
    calculate index stack = do
      start <- getCurrentTime
      print $ krivineMachine stack
      end <- getCurrentTime
      let res = "Amount of t's: "
                  ++ show (index + 1)
                  ++ ". Time to compute: "
                  ++ show (diffUTCTime end start)
      print res

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
  apply = foldl' (\ t x -> CApplication t (krivineMachine [x]))

compute :: String -> Either ParseError CTerm
compute = fmap krivineMachine . initialState
