module Krivine.Parallel where

import Krivine.Core (CTerm (..), Closure (..), ParseError, Stack, initialState, krivine)

import Control.Distributed.Process (Process, ProcessId, die, getSelfPid, liftIO, match, receiveWait,
                                    send, spawnLocal)
import Control.Distributed.Process.Node (initRemoteTable, newLocalNode, runProcess)
import Control.Monad (mapM_)

import Network.Transport.TCP (createTransport, defaultTCPParameters)

import Data.Either (either)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (foldl')
import Data.List.Index (imapM, imapM_, setAt)
import Data.Maybe (fromMaybe)

import Debug.Trace

import Data.Time

runParallelKrivine :: [Stack] -> IO ()
runParallelKrivine stacks = do
    let g "10501" = ("127.0.0.1", "10501")
    Right t <- createTransport "127.0.0.1" "10501" g defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runProcess node $ imapM_ calc stacks
  where
    calc index stack = do
      start <- liftIO getCurrentTime

      term <- krivineMachine stack
      liftIO $ print term

      end <- liftIO getCurrentTime
      let res = "Amount of t's: "
                  ++ show (index + 1)
                  ++ ". Time to compute: "
                  ++ show (diffUTCTime end start)
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
      Closure (Constant c) _ ->
        case xs of
          [] -> return $ Constant c
          _  -> computeParallel (Constant c) xs
      Closure (CVariable nu i) e ->
        case e of
          [] ->
            case xs of
              [] -> return $ CVariable nu i
              _  -> computeParallel (CVariable nu i) xs
          _ -> krivineMachine stack
      _ -> krivineMachine stack

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
      send pid (self, index, closure)

    handleResult :: Ref -> Process ()
    handleResult ref = do
      (index, term) <- receiveWait [match return]
      cur <- liftIO $ readIORef ref
      liftIO $ writeIORef ref (setAt index (Just term) cur)

    computeTerm :: (ProcessId, Int, Closure) -> Process ()
    computeTerm (sender, index, closure) = do
      term <- krivineMachine [closure]
      send sender (index, term)

compute :: String -> Either ParseError (Process CTerm)
compute = fmap krivineMachine . initialState
