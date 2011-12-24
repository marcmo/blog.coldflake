member :: (Ord a,Show a) => [a] -> Trie a -> Maybe [a]
member _ Empty = Nothing
member [] (Node b _) = b
member (x:xs) n@(Node b tr) = maybe Nothing (member xs) (M.lookup x tr)
