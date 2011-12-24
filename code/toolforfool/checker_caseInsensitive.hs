newtype CaseInsensitive = CI { content :: T.Text }
instance Eq CaseInsensitive where
  (==) (CI a) (CI b) = T.toCaseFold a == T.toCaseFold b
instance Ord CaseInsensitive where
  compare (CI a) (CI b) = compare (T.toCaseFold a) (T.toCaseFold b)
