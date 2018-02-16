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

makeEx' :: Int -> IO String
makeEx' n = build where
  buildIds = do
    n <- randomRIO (2000000, 2000100) :: IO Int
    return $ repeat' n "(\\xx)"

  terms = sequence [buildIds | _ <- [1..(n - 1)]]

  term = do
    ts <- terms
    return $ foldl (\res t -> res ++ t ++ "x)") "" ts

  build = do
    t <- term
    ids <- buildIds
    return $ repeat' n "(" ++ "x)" ++ t ++ ids ++ "x"

showEx' :: Int -> String
showEx' n = build where
  apps = repeat' n "y)"
  brackets = repeat' (n-1) "("
  build = "(\\y" ++ brackets ++ "(x)" ++ apps ++ "(\\xx)^{n}z " ++ "where n = 2000000"

makeEx :: Int -> Int -> String
makeEx n j = build where
  fs = repeat' n "(f)"
  fs' = repeat' j "(f)"
  build = "((\\x\\y((a)(x)y)(b)(y)x)\\f\\z" ++ fs ++ "z)\\f\\z" ++ fs' ++ "z"

showEx :: Int -> Int -> String
showEx n m = "((\\x\\y((a)(x)y)(b)(y)x)\\f\\z"
              ++ "(f)^{n}"
              ++ "z)\\f\\z"
              ++ "(f)^{m}"
              ++ "z Where n=" ++ show n
              ++ " and m=" ++ show m

repeat' :: Int -> String -> String
repeat' n str = concat [str | _ <- [1..n]]

delimiter :: IO ()
delimiter = print $ repeat' 150 "="

status :: String -> IO ()
status text = do
  delimiter
  print text
  delimiter


cases :: [(Int, Int)]
cases = [(6,6), (6,7), (6,8), (7, 6), (7,7), (7,8), (8, 6), (8, 7), (8,8)]

main :: IO ()
main = do
  let expExs = [makeEx n j | (n,j) <- cases]
  let expStacks = sequence [initialState ex | ex <- expExs ]
  let expExs = [showEx n j | (n, j) <- cases]


  stacks <- sequence [(fmap . fmap) initialState makeEx' n | n <- [1..8]]
  let exs = [showEx' n | n <- [1..8]]

  case sequence stacks of
    Left err     -> print err
    Right stacks -> do
      -- status "START EXPONENTIAL SEQUENTIAL BENCHMARK"
      -- runSequentialKrivine $ combine expExs (right expStacks)
      -- status "FINISH EXPONENTIAL SEQUENTIAL BENCHMARK"

      -- status "START IDS SEQUENTIAL BENCHMARK"
      -- runSequentialKrivine $ combine exs stacks
      -- status "FINISH IDS SEQUENTIAL BENCHMARK"


      -- status "START EXPONENTIAL PARALLEL BENCHMARK"
      -- runParallelKrivine $ combine expExs (right expStacks)
      -- status "FINISH EXPONENTIAL PARALLEL BENCHMARK"

      status "START IDS ACTORS BENCHMARK"
      runParallelKrivine $ combine exs stacks
      status "FINISH IDS ACTORS BENCHMARK"


      -- status "START EXPONENTIAL THREADS BENCHMARK"
      -- runParallelAsyncKrivine $ combine expExs (right expStacks)
      -- status "FINISH EXPONENTIAL THREADS BENCHMARK"

      -- status "START IDS THREADS BENCHMARK"
      -- runParallelAsyncKrivine $ combine exs stacks
      -- status "FINISH IDS THREADS BENCHMARK"


combine :: [a] -> [b] -> [(a, b)]
combine exs stacks = [(exs !! n, stacks !! n) | n <- [0..(length stacks - 1)]]

right :: Show a => Either a p -> p
right a =
  case a of
    Left err -> error (show err)
    Right r  -> r
