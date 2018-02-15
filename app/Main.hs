module Main where

import Krivine.Core
import Krivine.Parallel
import Krivine.ParallelAsync
import Krivine.Sequential

import System.Random


-- This function generates term of the form ((...(x)t)t..)t
-- All t of the form (\xx)..(\xx)x
-- Amount of ts are defined by parameter n
--
-- Example
-- makeEx 1 ~ (x)t
-- makeEx 2 ~ ((x)t)t

-- makeEx :: Int -> IO String
-- makeEx n = build where
--   idLength = 100000 `div` n

--   buildIds = do
--     n <- randomRIO (2000000, 2000010) :: IO Int
--     return $ repeat' n "(\\xx)"

--   terms = sequence [buildIds | _ <- [1..(n - 1)]]

--   -- ids = repeat' idLength "(\\xx)"
--   -- term = repeat' (n-1) (ids ++ "x)")
--   term = do
--     ts <- terms
--     return $ foldl (\res t -> res ++ t ++ "x)") "" ts

--   build = do
--     t <- term
--     ids <- buildIds
--     let res = repeat' n "(" ++ "x)" ++ t ++ ids ++ "x"
--     -- print $ length res
--     return res

makeEx :: Int -> String
makeEx n = build where
  fs = repeat' n "(f)"
  build = "((\\x\\y((a)(x)y)(b)(y)x)\\f\\z" ++ fs ++ "z)\\f\\z" ++ fs ++ "z"

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
  let stacks = [initialState $ makeEx n | n <- [1..8] ]
  -- stacks <- sequence [(fmap . fmap) initialState makeEx n | n <- [1..8]]
  case sequence stacks of
    Left err     -> print err
    Right stacks -> do
      status "FINISH PARSING TERMS"

      -- status "START SEQUENTIAL BENCHMARK"
      -- runSequentialKrivine stacks
      -- status "FINISH SEQUENTIAL BENCHMARK"

      -- status "START PARALLEL BENCHMARK"
      -- runParallelKrivine stacks
      -- status "FINISH PARALLEL BENCHMARK"

      status "START ASYNC PARALLEL BENCHMARK"
      runParallelAsyncKrivine stacks
      status "FINISH ASYNC PARALLEL BENCHMARK"
