import Control.Concurrent
import Control.Concurrent.MVar
import System.Environment
import Control.Monad

main = do
  mv <- newEmptyMVar
  [nrThreads] <- liftM (map read) getArgs
  tids <- replicateM nrThreads (forkIO $ operation mv)
  answer <- takeMVar mv
  mapM_ killThread tids

operation :: MVar Int -> IO ()
operation mv = (print "executing op") >> putMVar mv 5

