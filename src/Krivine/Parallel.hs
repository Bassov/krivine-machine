module Krivine.Parallel where

import Krivine.Core (CTerm (..), Closure (..), ParseError, Stack, initialState, krivine)

import Control.DeepSeq
import Control.Distributed.Process (Process, ProcessId, getSelfPid, liftIO, match, receiveWait,
                                    spawnLocal, unsafeSend)
import Control.Distributed.Process.Node (initRemoteTable, newLocalNode, runProcess)

import Network.Transport.TCP (createTransport, defaultTCPParameters)

import Data.Either (either)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (foldl')
import Data.List.Index (imapM_, setAt)
import Data.Maybe (fromMaybe)
import Data.Time

runParallelKrivine :: [(String, Stack)] -> IO ()
runParallelKrivine stacks = do
    let g "10501" = ("127.0.0.1", "10501")
    Right t <- createTransport "127.0.0.1" "10501" g defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runProcess node $ mapM_ calc stacks
  where
    calc (toCompute, stack) = do
      start <- liftIO getCurrentTime
      term <- krivineMachine stack
      end <- term `deepseq` liftIO getCurrentTime
      let res =   "Time to compute: "
                  ++ show (diffUTCTime end start)
      liftIO $ print $ "Term: " ++ toCompute
      liftIO $ print res

compute' :: String -> Process ()
compute' t =
  case compute t of
    Left ex -> liftIO $ print ex
    Right term -> do
      res <- term
      liftIO $ print res

krivineMachine :: Stack -> Process CTerm
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

  apply :: CTerm -> Stack -> Process CTerm
  apply t stack = do
    computations <- mapM (\cl -> krivineMachine [cl]) stack
    return $ foldl' CApplication t computations

computeParallel :: CTerm -> Stack -> Process CTerm
computeParallel t stack = do
  computations <- computeParalell' stack
  return $ foldl' CApplication t computations

type Ref = IORef [Maybe CTerm]

computeParalell' :: Stack -> Process [CTerm]
computeParalell' stack = do
    ref <- liftIO $ newIORef (map (const Nothing) stack)

    imapM_ calculate stack
    mapM_ (\_ -> handleResult ref) stack

    res <- liftIO $ readIORef ref
    return $ fromMaybe [] (sequence res)
  where
    calculate :: Int -> Closure -> Process ()
    calculate index closure = do
      self <- getSelfPid
      pid <- spawnLocal $ receiveWait [match computeTerm]
      unsafeSend pid (self, index, closure)

    handleResult :: Ref -> Process ()
    handleResult ref = do
      (index, term) <- receiveWait [match return]
      cur <- liftIO $ readIORef ref
      liftIO $ writeIORef ref (setAt index (Just term) cur)

    computeTerm :: (ProcessId, Int, Closure) -> Process ()
    computeTerm (sender, index, closure) = do
      term <- krivineMachine [closure]
      unsafeSend sender (index, term)

compute :: String -> Either ParseError (Process CTerm)
compute = fmap krivineMachine . initialState
