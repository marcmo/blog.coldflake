insert :: (Eq a,Ord a) => [a] -> Trie a -> Trie a
insert ss t = insert' [] ss t where
  insert' rs c Empty = insert' rs c (Node Nothing M.empty) 
  insert' rs [] (Node _ d) = Node (Just rs) d
  insert' rs (x:xs) n@(Node b dic)
    | x `M.member` dic = Node b (M.adjust (insert' (x:rs) xs) x dic)
    | True = Node b (M.insert x (insert' (x:rs) xs Empty) dic)

fromList = foldr insert Empty
