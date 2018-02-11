module Main where

import Control.Concurrent (threadDelay)
import Control.Distributed.Process (Process, ProcessId, die, expectTimeout, getSelfPid, liftIO,
                                    match, receiveWait, say, send, spawnLocal)
import Control.Distributed.Process.Node (initRemoteTable, newLocalNode, runProcess)
import Control.Monad (forever)
import Network.Transport.TCP (createTransport, defaultTCPParameters)

import KrivineMachine

ex1 :: String
ex1 = "(\\xx)y"

ex2 :: String
ex2 = "(\\xy)(\\x(x)x)\\x(x)x"

ex3 :: String
ex3 = "\\f(\\x(f)(x)x)\\x(f)(x)x"

computeTerm :: (ProcessId, String) -> Process ()
computeTerm (sender, term) = send sender $ (show . compute) term

receiveResult :: String -> Process ()
receiveResult res = liftIO $ print $ "got: " ++ res

mkProcess :: Process ProcessId
mkProcess = spawnLocal $ receiveWait [match computeTerm]

compute' :: String -> Process ()
compute' t = do
  self <- getSelfPid
  c <- mkProcess
  send c (self, t)
  receiveWait [match receiveResult]

main :: IO ()
main = do
  let g "10501" = ("127.0.0.1", "10501")
  Right t <- createTransport "127.0.0.1" "10501" g defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node $ do
    compute' ex1
    compute' ex2
