import qualified Data.IntMap as M
import Data.Char(chr,ord)
import Data.Array

type State = (Int,M.IntMap Int)
maxsize = 30000
data Dir =  Up | Down

interpret :: String -> IO ()
interpret input = f 0 (0,M.fromList $ zip [0..] (replicate maxsize 0)) where
  f :: Int -> State -> IO ()
  f pI (pD,dat)
    | pI == length input = print "done!" >> return ()
    | x == '>' = f (succ pI) (succ pD,dat)
    | x == '<' = f (succ pI) (pred pD,dat)
    | x == '+' = f (succ pI) (pD,update succ)
    | x == '-' = f (succ pI) (pD,update pred)
    | x == '.' = putChar (chr $ dat M.! pD) >> f (succ pI) (pD,dat)
    | x == ',' = getChar >>= \c -> f (succ pI) (pD,update (const (ord c)))
    | x == '[' && dat M.! pD == 0 = f (succ (matchUpTable M.! pI)) (pD,dat)
    | x == '[' = f (succ pI) (pD,dat)
    | x == ']' = f (matchDownTable M.! pI) (pD,dat)
    | otherwise = f (succ pI) (pD,dat)
    where instructions = listArray (0,length input-1) input
          x = instructions!pI
          update f = M.update (Just . f) pD dat
          matchUpTable = buildTable Up '[' instructions
          matchDownTable = buildTable Down ']' instructions

buildTable dir start instructions = M.fromList
  [(p,matching dir instructions p) | p <- [0..snd $ bounds instructions],
                                     instructions!p == start]

matching :: Dir -> Array Int Char -> Int -> Int 
matching Up instructions p = m (succ p) 0
  where m p depth
          | instructions!p == '[' = m (succ p) (depth+1)
          | instructions!p == ']' = if depth == 0 then p else m (succ p) (depth-1)
          | otherwise = m (succ p) depth
matching Down instructions p = m (pred p) 0
  where m p depth
          | instructions!p == ']' = m (pred p) (depth+1)
          | instructions!p == '[' = if depth == 0 then p else m (pred p) (depth-1)
          | otherwise = m (pred p) depth

main = getContents >>= interpret

