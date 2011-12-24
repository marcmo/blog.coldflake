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
