-- | Test if `rparWith r0` has the documented behaviour: to create a spark,
-- that does nothing. In particular the spark won't evaluate to weak head
-- normal form (WHNF).
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import System.IO.Unsafe (unsafePerformIO)

import Control.Parallel.Strategies

data LazyBox a = LazyBox a

main = do
  -- Create spark, that must *not* evaluate printWhenEvaluated
  LazyBox printWhenEvaluated <- evaluate $ runEval $ do
    let printWhenEvaluated = unsafePerformIO $ putStrLn "Must be printed at the end."
    sparkedResult <- rparWith r0 printWhenEvaluated
    return $ LazyBox sparkedResult

  -- Give the run time system enough time to evaluate the spark (~0.1s)
  threadDelay $ 10^5

  putStrLn "Must be printed first."

  -- Finally evaluate printWhenEvaluated
  evaluate printWhenEvaluated
