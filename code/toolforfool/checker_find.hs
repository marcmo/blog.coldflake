headerFiles = find always (extension ==? ".h")
headerAndSourceFiles = find always (extension ==? ".h" ||? extension ==? ".c")

toTrie ::  [String] -> Trie CaseInsensitive
toTrie hss = fromList [map CI ts | ts <- validPaths]
  where allPaths = map T.pack hss
        validPaths = concatMap reverseSplitAll allPaths
        reverseSplitAll = map reverse . L.tails . splitP
        splitP = map T.pack . splitDirectories . T.unpack
