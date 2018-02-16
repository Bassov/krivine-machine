module Krivine.ParallelAsync where

import Krivine.Core (CTerm (..), Closure (..), ParseError, Stack, initialState, krivine)

import Control.Concurrent.Async
import Control.DeepSeq
import Control.Monad.IO.Class

import Data.Either (either)
import Data.List (foldl')
import Data.Time

runParallelAsyncKrivine :: [(String, Stack)] -> IO ()
runParallelAsyncKrivine = mapM_ calc
  where
    calc (toCompute, stack) = do
      start <- liftIO getCurrentTime
      term <- krivineMachine stack
      end <- term `deepseq` liftIO getCurrentTime
      let res = "Time to compute: "
                  ++ show (diffUTCTime end start)
      liftIO $ print $ "Term: " ++ toCompute
      liftIO $ print res

compute' :: String -> IO ()
compute' t =
  case compute t of
    Left ex -> liftIO $ print ex
    Right term -> do
      res <- term
      liftIO $ print res

krivineMachine :: Stack -> IO CTerm
krivineMachine = either left right . krivine where
  left (k, cl) = do
    t <- krivineMachine [cl]
    return $ Lambda k t

  right stack@(cl:xs) =
    case cl of
      Closure (FreeVariable c) _ ->
        case xs of
          [] -> return $ FreeVariable c
          _  -> case length xs of
            1 -> apply (FreeVariable c) xs
            _ -> computeParallel (FreeVariable c) xs
      Closure (CVariable nu i) e ->
        case e of
          [] ->
            case xs of
              [] -> return $ CVariable nu i
              _  -> case length xs of
                1 -> apply (CVariable nu i) xs
                _ -> computeParallel (CVariable nu i) xs
          _ -> krivineMachine stack
      _ -> krivineMachine stack

  apply :: CTerm -> Stack -> IO CTerm
  apply t stack = do
    computations <- mapM (\cl -> krivineMachine [cl]) stack
    return $ foldl' CApplication t computations

computeParallel :: CTerm -> Stack -> IO CTerm
computeParallel t stack = do
  computations <- computeParalell' stack
  return $ foldl' CApplication t computations

computeParalell' :: Stack -> IO [CTerm]
computeParalell' = mapConcurrently (\cl -> krivineMachine [cl])

compute :: String -> Either ParseError (IO CTerm)
compute = fmap krivineMachine . initialState
