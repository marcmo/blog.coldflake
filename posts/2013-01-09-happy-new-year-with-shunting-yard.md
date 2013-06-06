---
title: Start of Year with Shunting Yard
description: applying dijkstras shunting yard algorithm for expression evaluation
tags: haskell, puzzle, algorithm, lua
---

![](/images/shuntingyard/rails2.jpg)

One of my resolutions for the new year is to solve more interesting problems and learn about new algorithms. The new year puzzle from [programming praxis] has some very interesting problem that I used as an excuse to try out one of Dijkstra's algorithms: [Shunting Yard].

> As we begin the new year, we note that 109-8\*7+654\*3-2/1 = 2013. There are three other combinations of the numbers 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, in order, combined with the five operators NIL, +, -, \* and / that also evaluate to 2013.
>
> Your task is to write a program that finds all four expressions that evaluate to 2013.

There are a couple of little challenges hidden in this problem:

* generating valid combinations of the input numbers
* evaluating those combinations (with correct precedence)
* possibly dealing with the complexity

My first approach was to use the fact that you can evaluate strings in lua directly. Here the only challenge is to generate suitable input strings:

~~~ {.lua}
local ops = {"","+","-","*","/"}
function check(n,e,res)
  e = e..n
  if n == 1 then
    if luaeval(e) == 2013 then table.insert(res,e) end
  else
    for i,v in ipairs(ops) do check(n-1, e..v,res) end
  end
  return res
end

function luaeval(e)
  return assert(loadstring("return "..e))()
end

local res = check(10,"",{})
for i,v in ipairs(res) do print(v,",") end
~~~

And seems to work correctly:

<pre class="terminal">
<span id="prompt">2013_01_01</span> > time lua combineNumbers.lua 
109-8*7+654*3-2*1	,
109-8*7+654*3-2/1	,
10*98/7*6/5*4*3-2-1	,
10*9*8*7/6/5*4*3-2-1	,

real	0m13.970s
user	0m13.910s
sys	0m0.019s
</pre>

This approach works if you use language or library that allows you to evaluate a string as an expression. But... somehow that felt kind of cheating and not very challenging. Building up the expression and evaluating it seemed a lot more fun.  
There are a couple of approaches you can take for expression evaluation, e.g.:

* build up a grammar and a [recursive descent parser] from scratch
* use some sort of parser generator like [bison] or [antlr]
* use the shunting yard algorithm

The latter was the approach I was not familiar with so I decided to go for that one.

## Shunting Yard

The basic idea of this algorithm is to go through the elements of your expression while keeping 2 stacks, one for the operators and the one for the operands. It plugs off tokens from the input and pushes the operands onto one stack and the operators onto a second. The stack of the operators has to be ordered by precedence so that the operator on the bottom has the lowest precedence, the one at the top the highest.  
In order to keep that order, all operators with higher precedence will have to be cleared of the stack (== applied to operands from the operand stack) until the current operator again has the highest precedence.  
Consider the example `a+b*c+d`:


<table>
  <tr>
    <td>input</td>
    <td>operands</td>
    <td>operators</td>
  </tr>
  <tr>
    <td>"a+b*c+d"</td>
    <td>[]</td>
    <td>[]</td>
  </tr>
  <tr>
    <td>"+b*c+d"</td>
    <td>[<b>a</b>]</td>
    <td>[]</td>
  </tr>
  <tr>
    <td>"b*c+d"</td>
    <td>[a]</td>
    <td>[<b>+</b>]</td>
  </tr>
  <tr>
    <td>"*c+d"</td>
    <td>[<b>b</b>:a]</td>
    <td>[+]</td>
  </tr>
  <tr>
    <td>"c+d"</td>
    <td>[b:a]</td>
    <td>[<b>*</b>:+]</td>
  </tr>
  <tr>
    <td>"+d"</td>
    <td>[<b>c</b>:b:a]</td>
    <td>[*:+]</td>
  </tr>
  <tr>
    <td>"+d"</td>
    <td>[<b>(c*b)</b>:a]</td>
    <td>[*:+]</td>
  </tr>
  <tr>
    <td>"d"</td>
    <td>[(c*b):a]</td>
    <td>[<b>+</b>:+]</td>
  </tr>
  <tr>
    <td>""</td>
    <td>[<b>d</b>:(c*b):a]</td>
    <td>[+:+]</td>
  </tr>
  <tr>
    <td>""</td>
    <td>[<b>d+(c*b)</b>:a]</td>
    <td>[+]</td>
  </tr>
  <tr>
    <td>""</td>
    <td>[<b>d+(c*b)+a</b>]</td>
    <td>[]</td>
  </tr>
</table>

## A haskell implementation

First we need some datatypes to represent the operators:

~~~ {.haskell}
data Operator = Plus | Minus | Div | Mult | None | Id Double deriving(Eq,Show)
~~~

The shunting yard algorithm translates nicely:

~~~ {.haskell}
eval :: [Operator] -> Double
eval xs = shunting xs [] []
  where shunting [] [v] _ = v
        shunting (Id n:ns) as os = shunting ns (n:as) os
        shunting [] (a:b:bs) (o:os) = shunting [] (apply o b a:bs) os
        shunting (n:ns) as [] = shunting ns as [n]
        shunting (n:ns) (a:b:bs) (o:os)
          | n `higher` o = shunting ns (a:b:bs) (n:o:os)
          | otherwise = shunting (n:ns) (apply o b a:bs) os
~~~

`apply` just applies an operator to it's respective 2 operands and the `higher` function indicates the relative precedence:

~~~ {.haskell}
apply :: Operator -> Double -> Double -> Double
apply None a b = 10*a+b
apply Mult a b = a*b
apply Div a b = a/b
apply Plus a b = a+b
apply Minus a b = a-b

higher :: Operator -> Operator -> Bool
higher None None = False
higher None _ = True
higher Div Plus = True
higher Div Minus = True
higher Mult Plus = True
higher Mult Minus = True
higher _ _ = False
~~~

~~~ {.haskell}
apply :: Operator -> Double -> Double -> Double
~~~
Now that we can evaluate expression, the only thing that remains is to generate all possible expressions and filter out the valid one:

~~~ {.haskell}
combinations n = sequence . replicate n

interleave (n:ns) cs =
    reverse $ foldl' (\acc (a,b) -> a:b:acc) [n] (zip ns cs)

solve n = [combo | combo <- map (interleave ids) (combinations 9 operators)
                 , eval combo == n]
  where ids = [Id n | n <- reverse [1..10]]
~~~

Turns out for the size of the problem it is quite possible to check all combinations (we have to use 9 operators, for each 5 possibilities, so there are 5^9 ~ 2 mio. combinations). Running the code on my Macbook Pro 2.3 GHz Core i5 seems fast enough:

<pre class="terminal">
<span id="prompt">2013_01_01</span> > time ./combineNumbers

[[Id 10.0,Mult,Id 9.0,Mult,Id 8.0,Mult,Id 7.0,Div,Id 6.0,Div,Id 5.0,Mult,Id 4.0,Mult,Id 3.0,Minus,Id 2.0,Minus,Id 1.0],
[Id 10.0,Mult,Id 9.0,None,Id 8.0,Div,Id 7.0,Mult,Id 6.0,Div,Id 5.0,Mult,Id 4.0,Mult,Id 3.0,Minus,Id 2.0,Minus,Id 1.0],
[Id 10.0,None,Id 9.0,Minus,Id 8.0,Mult,Id 7.0,Plus,Id 6.0,None,Id 5.0,None,Id 4.0,Mult,Id 3.0,Minus,Id 2.0,Div,Id 1.0],
[Id 10.0,None,Id 9.0,Minus,Id 8.0,Mult,Id 7.0,Plus,Id 6.0,None,Id 5.0,None,Id 4.0,Mult,Id 3.0,Minus,Id 2.0,Mult,Id 1.0]]

real	0m1.928s
user	0m1.798s
sys	0m0.055s
</pre>

Full source code as usual available as a [gist].

<citation>[Photo: Thomas Ulrich/Pixbay]  [(creativecommons)]</citation>

[programming praxis]:http://programmingpraxis.com/2013/01/01/happy-new-year/
[Shunting Yard]:http://en.wikipedia.org/wiki/Shunting-yard_algorithm
[recursive descent parser]:http://en.wikipedia.org/wiki/Recursive_descent_parser
[bison]:http://www.gnu.org/software/bison/
[antlr]:http://www.antlr.org/
[(creativecommons)]:http://creativecommons.org/publicdomain/zero/1.0/deed.en
[Photo: Thomas Ulrich/Pixbay]:http://pixabay.com/en/users/LoboStudioHamburg/
[gist]:https://gist.github.com/4493943


