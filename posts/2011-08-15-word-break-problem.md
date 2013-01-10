---
title: Lunch break coding - Word Break Problem
description: simple programming puzzle to break up words according to dictionary
tags: haskell, puzzle, algorithm
---

Another funny little programming puzzle that I found [here](http://programmingpraxis.com/2011/08/12/word-breaks/) describes the _Word Break Problem_.

> Given an input string and a dictionary of words, segment the input string into a space-separated sequence of dictionary words if possible. For example, if the input string is “applepie” and dictionary contains a standard set of English words, then we would return the string “apple pie” as output. 

So let's give it a shot at using haskell:

First we need some imports and a convenient data type for the dictionary that we are fed:

~~~ {.haskell}
import Data.List(inits)
import qualified Data.Set as S
import Maybe(isJust)
import Control.Applicative((<$>))
import System(getArgs)

type Dictionary = S.Set String
~~~

Next the formulation of the problem itself:

~~~ {.haskell}
breakWords ::  String -> Dictionary -> Maybe [String]
breakWords xs dict = splittUp xs [] where
  splittUp :: String -> [String] -> Maybe [String]
  splittUp [] rs = Just $ reverse rs -- done, give back result
  splittUp xs rs = safeHead $ filter isJust solutions where
    solutions = [splittUp rest (match:rs) | (match,rest) <- findMatches xs dict]
~~~

And of course the helper functions we need for this to work:

~~~ {.haskell}
findMatches :: String -> Dictionary -> [(String,String)]
findMatches xs dict =
  [(x,drop (length x) xs) | x <- reverse $ inits xs, x `S.member` dict]
          
safeHead xs = if null xs then Nothing else head xs
~~~

Now drop into _ghci_ for a quick test to validate the implementation:

<pre class="terminal">
*Main> S.fromList . lines <$> readFile "/usr/share/dict/words" >>=
          print . breakWords "sometimesitisjustsosimple"
Just ["sometimes","it","is","just","so","simple"]
</pre>

Seems to work alright!  
This implementation will give us exactly one valid solution (trying first to find the largest possible matches).  
If we are interested in *all* possibilities, this is just a small tweak:

~~~ {.haskell}
breakWords2 xs dict = splittUp xs [] where
  splittUp [] rs = [Just $ reverse rs] -- done, give back result
  splittUp xs rs = filter isJust $ concat solutions where
    solutions = [splittUp rest (match:rs) | (match,rest) <- findMatches xs dict]
~~~

Full source-code is available [here](/code/wordbreakproblem/full.hs).
