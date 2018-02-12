module Main where

import Criterion.Main
import Krivine.Parallel
import Krivine.Sequential

-- Our benchmark harness.
main = do
  runSequentialKrivine
  runParallelKrivine
