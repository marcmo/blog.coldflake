
type TrieMap a = M.Map a (Trie a)
data Trie a = Empty | Node (Maybe [a]) (TrieMap a) deriving (Show)

insert :: (Eq a,Ord a) => [a] -> Trie a -> Trie a
insert ss t = insert' [] ss t where
  insert' rs c Empty = insert' rs c (Node Nothing M.empty) 
  insert' rs [] (Node _ d) = Node (Just rs) d
  insert' rs (x:xs) n@(Node b dic)
    | x `M.member` dic = Node b (M.adjust (insert' (x:rs) xs) x dic)
    | True = Node b (M.insert x (insert' (x:rs) xs Empty) dic)

member :: (Ord a,Show a) => [a] -> Trie a -> Maybe [a]
member _ Empty = Nothing
member [] (Node b _) = b
member (x:xs) n@(Node b tr) = maybe Nothing (member xs) (M.lookup x tr)

fromList = foldr insert Empty
