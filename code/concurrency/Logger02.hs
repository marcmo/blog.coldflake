import Control.Concurrent
import Control.Concurrent.MVar
import System.Environment
import Control.Monad

startLogger :: IO ( String -> IO ())
startLogger = do
  chan <- newChan
  forkIO ( forever ( readChan chan >>= putStrLn ))
  return ( writeChan chan )

example :: IO ()
example = do
  lg <- startLogger
  lg " Hello , world ! "

