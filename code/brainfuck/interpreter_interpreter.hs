interprete :: String -> IO ()
interprete input = f 0 (0,M.fromList $ zip [0..] (replicate maxsize 0)) where
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

