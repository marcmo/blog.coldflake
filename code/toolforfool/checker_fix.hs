fixIncludes :: BSC.ByteString -> Trie CaseInsensitive -> Maybe BSC.ByteString
fixIncludes content availablePaths
  | n == 0 = Nothing
  | otherwise = Just $ BSC.unlines $ reverse finalLines
  where allLines = BSC.lines content
        (finalLines,n) = foldl' maybeFixLine ([],0) allLines
        maybeFixLine (acc,nn) x =
          maybe (x:acc,nn)
                (\f->(E.encodeUtf8 f:acc,nn+1))
                (fixForLine x)
        fixForLine x 
          | BSC.unpack x =~ "^#include[ ]{1,}[<\"]" =
              m >>= \s -> return (T.concat ["#include ",start,s,end])
          | otherwise = Nothing
          where m = bestMatch cut availablePaths
                (cut,start,end) = cutIncludePath (E.decodeUtf8 x)

cutIncludePath :: T.Text -> (T.Text,T.Text,T.Text)
cutIncludePath i 
    | T.unpack i =~ "^#include[ ]{1,}\"" = ((head . tail . T.split (== '"')) i,"\"","\"")                                                     
    | T.unpack i =~ "^#include[ ]{1,}<"  =
        (T.dropAround (\c->(c == '<') || (c == '>')) (fst $ T.breakOn ">" $ snd $ T.breakOn "<" i),"<",">")
    | otherwise = error $ "no match for include path" ++ T.unpack i

