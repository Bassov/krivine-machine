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
import Data.List.Index (imapM_, setAt)
import Data.Maybe (fromMaybe)

import Debug.Trace

import Criterion.Main

ex1 :: String
ex1 = "(\\xx)y"

ex2 :: String
ex2 = "(((x)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)z)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)z)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)(\\xx)z"

runParallelKrivine :: IO ()
runParallelKrivine = do
  let g "10501" = ("127.0.0.1", "10501")
  Right t <- createTransport "127.0.0.1" "10501" g defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node $
    -- compute' ex1
    -- compute' ex2
    -- die "zhopa"
      liftIO $ defaultMain [
        bgroup "parallel" [ bench "1" $ whnf compute ex1
                          , bench "2" $ whnf compute ex2
                          ]
        ]

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
      c <- trace "parallel" spawnLocal $ receiveWait [match computeTerm]
      send c (self, index, closure)

    handleResult :: Ref -> Process ()
    handleResult ref = do
      (index, term) <- receiveWait [match return]
      cur <- liftIO $ readIORef ref
      liftIO $ writeIORef ref (setAt index (Just term) cur)

    -- mkProcess :: Process ProcessId
    -- mkProcess = (spawnLocal $ receiveWait [match computeTerm]

    computeTerm :: (ProcessId, Int, Closure) -> Process ()
    computeTerm (sender, index, closure) = do
      term <- krivineMachine [closure]
      send sender (index, term)

compute :: String -> Either ParseError (Process CTerm)
compute = fmap krivineMachine . initialState
