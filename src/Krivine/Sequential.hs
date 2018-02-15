module Krivine.Sequential where

import Data.Either (either)
import Krivine.Core (CTerm (..), Closure (..), ParseError, Stack, initialState, krivine)

import Data.List
import Data.Time

import Control.DeepSeq

runSequentialKrivine :: [Stack] -> IO ()
runSequentialKrivine = mapM_ calculate
  where
    calculate stack = do
      start <- getCurrentTime
      let term =  krivineMachine stack
      print $ length (show term)
      -- print term
      end <- term `deepseq` getCurrentTime
      let res =  "Time to compute: "
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
