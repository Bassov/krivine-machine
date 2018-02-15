module Main where

import Krivine.Core
import Krivine.Parallel
import Krivine.Sequential


-- This function generates term of the form ((...(x)t)t..)t
-- All t of the form (\xx)..(\xx)x
-- Amount of ts are defined by parameter n, and ammount of all (\xx) accros the term
-- is simillar to constant used in expression
--
-- Example
-- makeEx 1 ~ (x)t
-- makeEx 2 ~ ((x)t)t
-- But two of this terms has simmilar amount of (\xx) in ts
-- So it should take simmilar amount of time to compute the term
-- for sequential implementation of the Krivine Machine
makeEx :: Int -> String
makeEx n = build where
  idLength = 6000000 `div` n
  ids = repeat' idLength "(\\xx)"
  term = repeat' (n-1) (ids ++ "x)")
  build = repeat' n "(" ++ "x)" ++ term ++ ids ++ "x"

repeat' :: Int -> String -> String
repeat' n str = concat [str | _ <- [1..n]]

delimiter :: IO ()
delimiter = print $ repeat' 150 "="

status :: String -> IO ()
status text = do
  delimiter
  print text
  delimiter

main :: IO ()
main = do
  let stacks = sequence [initialState . makeEx $ n | n <- [1..8]]
  case stacks of
    Left err     -> print err
    Right stacks -> do
      status "FINISH PARSING TERMS"

      status "START SEQUENTIAL BENCHMARK"
      runSequentialKrivine stacks
      status "FINISH SEQUENTIAL BENCHMARK"

      status "START PARALLEL BENCHMARK"
      runParallelKrivine stacks
      status "FINISH PARALLEL BENCHMARK"
