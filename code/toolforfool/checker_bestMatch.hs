bestMatch ::  T.Text -> Trie CaseInsensitive -> Maybe T.Text
bestMatch include validCaseTrie = member reversedIncludeCI validCaseTrie >>=
    \matchedPath -> 
      if (content <$> matchedPath) == (content <$> includeCI)
      	then Nothing -- include path was spelled correctly
        else Just $ T.intercalate "/" [content x | x <- matchedPath]
    where
      reversedIncludeCI = map CI (reverse $ splitP include)
      includeCI = map CI (splitP include)
      splitP = map T.pack . splitDirectories . T.unpack
