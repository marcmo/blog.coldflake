---
title: A Tool for a Fool
description: correcting misspelled header file include lines
tags: haskell, C++, tool
---

It has been a while since I was foolish enough to convince some of my colleagues to use Linux as their primary OS instead of Windows. By now it has become much easier to ensure everything important runs on the Linux boxes. But the development team of a recent project was quite large (>80 devs!) and most of those still work in a Windows environment.  
So it should be no surprise that there will be incompatibilities in the source code, namely the spelling. Why is it so difficult to accept that `flash/flashdriver.h` is **not** the same as `flash/FlashDriver.h`?? I got sick of manually correcting those glitches so I wrote a tool for myself that will correct this automatically.

What the program basically does is it will go through each source file and correct any wrongly spelled include path it finds. To do that it will need to know of all available header files. Of course the include path itself can always be relative so it's mandatory that an include line like `#include "testA.h"` will match against a header file `one/two/testA.h`. You get the idea. Kind of like matching up the suffixes of strings.

## Trying Tries

In order to store the available header files I need some kind of a map which allows me to quickly determine if an include path exists with a different case or not. A data-structure that seems suitable for this (described in Chris Okasaki's excellent [book about purely functional data structures](http://www.amazon.com/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504)) is a Trie. Tries are based on ordered trees and can function as associative arrays where the keys are represented bye strings of tokens. Sounds kind of useful my scenario...yes it's also possible to use regular Hashmaps but what the heck...let's implement it with tries, just for the fun of it.
All found paths will be stored in the trie in reverse order so that the first level of edges will match the headerfile names. Descending further into the tree each edge will represent a path element (directory).

![](/images/toolforfool/trie_c.png)

This example represents the entries `["a.h","a/a.h","s/a.h","b.h","u/b.h","c.h","u/c.h"]` where we store the correctly spelled path along the edges to a node inside the node.  
This is the datatype definition for our trie, where we store the path in the nodes if it is valid.  

~~~ {.haskell}
type TrieMap a = M.Map a (Trie a)
data Trie a = Empty | Node (Maybe [a]) (TrieMap a) deriving (Show)
~~~

To insert a list of elements into the trie, we consecutively let each element trickle down the nodes until all elements are in.  
Building a trie from a list of elements just needs folding over the elements and inserting them.

~~~ {.haskell}
insert :: (Eq a,Ord a) => [a] -> Trie a -> Trie a
insert ss t = insert' [] ss t where
  insert' rs c Empty = insert' rs c (Node Nothing M.empty) 
  insert' rs [] (Node _ d) = Node (Just rs) d
  insert' rs (x:xs) n@(Node b dic)
    | x `M.member` dic = Node b (M.adjust (insert' (x:rs) xs) x dic)
    | True = Node b (M.insert x (insert' (x:rs) xs Empty) dic)

fromList = foldr insert Empty
~~~

Finally we need to be able to find out if a certain path (list of path-elements) is a member of the trie:

~~~ {.haskell}
member :: (Ord a,Show a) => [a] -> Trie a -> Maybe [a]
member _ Empty = Nothing
member [] (Node b _) = b
member (x:xs) n@(Node b tr) = maybe Nothing (member xs) (M.lookup x tr)
~~~

## Making the Case

The question our trie has to answer is whether an include path is actually present (even if spelled incorrectly) or not. Is any form of the path `foo/bar.h` available? Maybe `FOO/BAR.h`? And if it is, what is the correct spelling?  
So the `member` predicate has to return `True` even if we got the case wrong. Seems we need some kind of case-insensitive path representation:

~~~ {.haskell}
newtype CaseInsensitive = CI { content :: T.Text }
instance Eq CaseInsensitive where
  (==) (CI a) (CI b) = T.toCaseFold a == T.toCaseFold b
instance Ord CaseInsensitive where
  compare (CI a) (CI b) = compare (T.toCaseFold a) (T.toCaseFold b)
~~~

To build up the initial trie and a list of files that potentially need to be fixed we can make use of the excellent `FileManipCompat` package. It makes it easy to find all header and source files that are available.  
The headers will be used for the trie in such a way that s.th. like `a/b/c.h` will result in 3 valid paths: `c.h`, `b/c.h` and `a/b/c.h`.

~~~ {.haskell}
headerFiles = find always (extension ==? ".h")
headerAndSourceFiles = find always (extension ==? ".h" ||? extension ==? ".c")

toTrie ::  [String] -> Trie CaseInsensitive
toTrie hss = fromList [map CI ts | ts <- validPaths]
  where allPaths = map T.pack hss
        validPaths = concatMap reverseSplitAll allPaths
        reverseSplitAll = map reverse . L.tails . splitP
        splitP = map T.pack . splitDirectories . T.unpack
~~~

Now everything is in place that we can actually go ahead and try to fix broken include lines. This can be implemented as a pure function that takes the content of a file, filters out all `#include` lines and tries to find a more correct version for the include path.

~~~ {.haskell}
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

~~~

## Pick the Correct Version

The meat of the algorithm to find the better version will finally make use of the assembled trie. If the path was found, we check if our path is correctly spelled and if not use the correct path that is stored in the node of the trie.

~~~ {.haskell}
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
~~~

Is that gonna be fast enough for my code base? Let's see, how much code are we talking about:

<pre class="terminal">
$ find . -regex ".*\.\(cpp\|c\|h\|hpp\)" | wc -l
6639
$ find . -regex ".*\.\(cpp\|c\|h\|hpp\)" | xargs -n1 wc -l | cut -f1 -d'.' | paste -sd+ - | bc
1473908
</pre>

Almost 7000 header and source code files with approximately 1.5 Mio loc. Running the compiled version of the program with this code base takes about 4.5 seconds on my machine (2.40 GHz Quad-Core running Linux kernel 3.0.0). That will do for my part.

Full source code is available as a [gist](https://gist.github.com/1387109).
