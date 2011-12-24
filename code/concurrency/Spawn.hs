import Control.Concurrent
import Control.Concurrent.MVar
import System.Environment
import Control.Monad
import Network.HTTP.Enumerator
import qualified Data.ByteString.Lazy as L

spawn :: IO a -> IO ( IO a )
spawn body = do
  v <- newEmptyMVar
  forkIO ( body >>= putMVar v )
  return ( readMVar v )

example :: IO ()
example = do
  thr <- spawn (simpleHttp "http://www.haskell.org/")
  print "do other things concurrently..."
  result <- thr
  L.putStr result


