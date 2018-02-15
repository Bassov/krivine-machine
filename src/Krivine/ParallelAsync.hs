module Krivine.ParallelAsync where

import Krivine.Core (CTerm (..), Closure (..), ParseError, Stack, initialState, krivine)

import Control.Concurrent.Async
import Control.DeepSeq
import Control.Monad.IO.Class

import Data.Either (either)
import Data.List (foldl')
import Data.Time

runParallelAsyncKrivine :: [Stack] -> IO ()
runParallelAsyncKrivine = mapM_ calc
  where
    calc stack = do
      start <- liftIO getCurrentTime

      term <- krivineMachine stack
      liftIO $ print $ length (show term)
      -- liftIO $ print term

      end <- term `deepseq` liftIO getCurrentTime
      let res = "Time to compute: "
                  ++ show (diffUTCTime end start)
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
      Closure (Constant c) _ ->
        case xs of
          [] -> return $ Constant c
          _  -> case length xs of
            1 -> apply (Constant c) xs
            _ -> computeParallel (Constant c) xs
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
  print $ "start parallel, terms: " ++ show (length stack)
  start <- getCurrentTime

  computations <- computeParalell' stack

  end <- liftIO getCurrentTime
  print $ "end parallel, time: " ++ show (diffUTCTime end start)

  return $ foldl' CApplication t computations

computeParalell' :: Stack -> IO [CTerm]
computeParalell' = mapConcurrently (\cl -> krivineMachine [cl])

compute :: String -> Either ParseError (IO CTerm)
compute = fmap krivineMachine . initialState
