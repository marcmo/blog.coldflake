---
title: Lunch break coding - First Nonrepeating Character
description: simple programming puzzle
tags: haskell, puzzle, algorithm
---

Again a little programming task frome [here](http://programmingpraxis.com/2011/08/19/first-non-repeating-character/):

> Write a function that takes an input string and returns the first character from the string that is not repeated later in the string. For instance, the two letters “d” and “f” in the input string “aabcbcdeef” are non-repeating, and the function should return “d” since “f” appears later in the string. The function should return some indication if there are no non-repeating characters in the string. 

Seems like `Data.List` should have everything we need for a simple solution.

~~~ {.haskell}
import Data.List(group,find,sort)
~~~

We are given a string of character and first filter out the characters that are unique.  
Once those are assembled, we can recursively walk over the characters in the input string and find the first character that is unique:

~~~ {.haskell}
firstNonRepeating ::  Ord a => [a] -> Maybe a
firstNonRepeating xs = walkTill uniques xs where
  uniques = [head ys | ys <- group $ sort xs
                     , ((==) 1 . length) ys ]
  walkTill _ [] = Nothing
  walkTill uniques (x:xs)
    | x `elem` uniques = Just x
    | otherwise = walkTill uniques xs
~~~

A quick test to verify the algorithm:

<pre class="terminal">
*Main> firstNonRepeating "aabcbcdeef"
Just 'd'
*Main> firstNonRepeating "a................b..................c...................d"
Just 'a'
*Main> firstNonRepeating "aa....bb.....cc.....dd"
Nothing
</pre>

Full source-code is available [here](/code/firstNonrepeating/algorithm.hs).
