type TrieMap a = M.Map a (Trie a)
data Trie a = Empty | Node (Maybe [a]) (TrieMap a) deriving (Show)
