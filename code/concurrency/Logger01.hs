import Control.Concurrent
import Control.Concurrent.MVar
import System.Environment
import Control.Monad

data Logger = MkLogger ( Chan String )

startLogger :: IO Logger
startLogger = do
  chan <- newChan
  forkIO ( forever ( readChan chan >>= putStrLn ))
  return ( MkLogger chan )

writeMessage :: Logger -> String -> IO ()
writeMessage ( MkLogger chan ) msg = writeChan chan msg

example :: IO ()
example = do
  lg <- startLogger
  writeMessage lg " Hello , world ! "

