module Main (main) where

import Control.Concurrent (threadDelay)
import System.IO.Unsafe (unsafePerformIO)

import Control.Parallel.Strategies

printOnEvaluation :: String -> Eval ()
printOnEvaluation = return . unsafePerformIO . putStrLn

main :: IO Int
main = do
  nils <- runEvalIO $ do
    plast <- parEval $ printOnEvaluation "Printed last"
    _ <- parEval $ printOnEvaluation "Must not be printed"
    p1 <- parEval . rseq =<< printOnEvaluation "Printed 1st"
    return [plast, p1]

  -- wait for sparks
  threadDelay $ 10 ^ (5::Int)

  _ <- runEvalIO $ evalList rseq nils

  return 0

