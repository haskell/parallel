-- Checks if runEvalIO sequence the evaluation with the IO actions as expected.

import Control.Parallel.Strategies
import Control.Concurrent
import System.IO.Unsafe

longrunning :: Int -> IO ()
longrunning decisecs = do
  threadDelay $ 10^(5::Int) * decisecs
  putStr ("Thread has been delayed by " ++ shows decisecs "/10 seconds.\n")

interleavedPrint :: String -> ()
interleavedPrint = unsafePerformIO . putStrLn

main :: IO ()
main = do
  -- check runEvalIO . r0
  lastused <- runEvalIO . r0 
                $ interleavedPrint "Should be printed at the end."
  _ <- runEvalIO . r0 $ interleavedPrint "Should never be printed."

  -- check runEvalIO . rseq
  _ <- runEvalIO . rseq $ interleavedPrint "Should be printed first."

  -- check runEvalIO . rpar
  --
  -- When using the threaded runtime the spark should print its text before
  -- the main thread. If the unthreaded runtime is used it should never be
  -- printed.
  unit <- runEvalIO . rpar . unsafePerformIO $ longrunning 1
  longrunning 2
  return unit -- make sure unit is not garbage collected

  return $! lastused -- force printing now
