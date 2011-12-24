---
title: Lunch break coding - brainfuck
description: prolog problem solved in haskell
tags: haskell, puzzle
---

This is probably the worst name for a programming language after Javascript... but it's a really fun little language! The task is to [write an interpreter for it](http://programmingpraxis.com/2011/10/04/brainfuck/).

Usually I'd resort back to the trusty [parsec](http://legacy.cs.uu.nl/daan/parsec.html) library but it seems that this language is simple enough to do without.
This is the specification taken from the [wikipedia page](http://en.wikipedia.org/wiki/Brainfuck):

> Character	Meaning

> `>`	increment the data pointer (to point to the next cell to the right).  
> `<`	decrement the data pointer (to point to the next cell to the left).  
> `+`	increment (increase by one) the byte at the data pointer.  
> `-`	decrement (decrease by one) the byte at the data pointer.  
> `.`	output a character, the ASCII value of which being the byte at the data pointer.  
> `,`	accept one byte of input, storing its value in the byte at the data pointer.  
> `[`	if the byte at the data pointer is zero, then instead of moving the instruction pointer forward to the next command, jump it forward to the command after the matching ] command`*`.  
> `]`	if the byte at the data pointer is nonzero, then instead of moving the instruction pointer forward to the next command, jump it back to the command after the matching [ command`*`.

So let's add a few module imports and data types that we will need:

~~~ {.haskell}
import qualified Data.IntMap as M
import Data.Char(chr,ord)
import Data.Array

type State = (Int,M.IntMap Int)
maxsize = 30000
data Dir =  Up | Down
~~~

The main algorithm is just a loop that consumes the instruction input string one character at a time and acts upon the data area. Thus we need to keep track of both the current instruction (`instructions!pI`) and the state of the data area (the content `dat` and our position `pD`).  
We keep the instructions in an array and use an `IntMap` to represent the data-area in a way so we can easily update and traverse it. The `State` that we keep for the recursive invocations always tells us the current data-cell and date-area.

~~~ {.haskell}
interprete :: String -> IO ()
interprete input = f 0 (0,M.fromList $ zip [0..] (replicate maxsize 0)) where
  f :: Int -> State -> IO ()
  f pI (pD,dat)
    | pI == length input = print "done!" >> return ()
    | x == '>' = f (succ pI) (succ pD,dat)
    | x == '<' = f (succ pI) (pred pD,dat)
    | x == '+' = f (succ pI) (pD,update succ)
    | x == '-' = f (succ pI) (pD,update pred)
    | x == '.' = putChar (chr $ dat M.! pD) >> f (succ pI) (pD,dat)
    | x == ',' = getChar >>= \c -> f (succ pI) (pD,update (const (ord c)))
    | x == '[' && dat M.! pD == 0 = f (succ (matchUpTable M.! pI)) (pD,dat)
    | x == '[' = f (succ pI) (pD,dat)
    | x == ']' = f (matchDownTable M.! pI) (pD,dat)
    | otherwise = f (succ pI) (pD,dat)
    where instructions = listArray (0,length input-1) input
          x = instructions!pI
          update f = M.update (Just . f) pD dat
          matchUpTable = buildTable Up '[' instructions
          matchDownTable = buildTable Down ']' instructions
~~~

The only tricky part here is to find out where to jump to in case we need to execute a code jump to a matching bracket. Since those jumps will potentially over and over again we lazily build up a table that keeps track of matching brackets.  
The `matching` function calculates the position of an opening or closing bracket.

~~~ {.haskell}
buildTable dir start instructions = M.fromList
  [(p,matching dir instructions p) | p <- [0..snd $ bounds instructions],
                                     instructions!p == start]

matching :: Dir -> Array Int Char -> Int -> Int 
matching Up instructions p = m (succ p) 0
  where m p depth
          | instructions!p == '[' = m (succ p) (depth+1)
          | instructions!p == ']' = if depth == 0 then p else m (succ p) (depth-1)
          | otherwise = m (succ p) depth
matching Down instructions p = m (pred p) 0
  where m p depth
          | instructions!p == ']' = m (pred p) (depth+1)
          | instructions!p == '[' = if depth == 0 then p else m (pred p) (depth-1)
          | otherwise = m (pred p) depth
~~~

The last bit is to provide a main function so we can easily test the implementation:

~~~ {.haskell}
main = getContents >>= interprete
~~~

So let's do some tests. First an easy one, HelloWorld.bf:

~~~ {.haskell}
++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<++
+++++++++++++.>.+++.------.--------.>+.>.
~~~

Running this will give us:

<pre class="terminal">
$ ./interpreter &lt; HelloWorld.bf
Hello World!
"done!"
</pre>

And one more, a little less trivial, factorial.bf:

~~~ {.haskell}
+++++++++++++++++++++++++++++++++>++++++++++++++++++++++++++++++++++++
+++++++++++++++++++++++++>++++++++++>+++++++++>>+<<[>+++++++++++++++++
+++++++++++++++++++++++++++++++.--------------------------------------
----------<<<<.-.>.<.+>>>>>>>++++++++++<<[->+>-[>+>>]>[+[-<+>]>+>>]<<<
<<<]>[<+>-]>[-]>>>++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<]>[-]>>[++++
++++++++++++++++++++++++++++++++++++++++++++.[-]]<[+++++++++++++++++++
+++++++++++++++++++++++++++++.[-]]<<<+++++++++++++++++++++++++++++++++
+++++++++++++++.[-]<<<<<<.>>+>[>>+<<-]>>[<<<[>+>+<<-]>>[<<+>>-]>-]<<<<
-]
~~~

will result in:

<pre class="terminal">
$ time ./interpreter &lt; factorial.bf
0! = 1
1! = 1
2! = 2
3! = 6
4! = 24
5! = 120
6! = 720
7! = b40
8! = ǃ20
"done!"
0.96user 0.01system 0:00.97elapsed 99%CPU
</pre>

If anybody is interested in how these examples work I can recommend the javascript powered [brainfuck interpreter & debugger](http://www.lordalcol.com/brainfuckjs/).  
Good for today...  

Full source-code is available [here](/code/brainfuck/interpreter.hs).
