---
title: Scripting Git with Ghc Evaluation Mode
description: Some examples that show the usage of haskell's evaluation mode.
tags: haskell, git, ghc
---

Processing bits and pieces on the command line has a very functional feel to it. You feed some input to a function which gives you a result that again can be fed to another function and so on. Each of the individual processing steps resembles a pure function that takes some text as input and outputs some text again.  
One nice little command I keep in my `.bashrc` gives a quick overview of the size of all files and folders in the current directory.

<pre class="terminal">
$ du -s ./* | sort -n | cut -f 2- | xargs du -sh
</pre>

What this does is

1. get a list of the sizes of all files and folders in the current directory, including their names
2. sort this list by sizes
3. extract only the names (which now are ordered according the size)
4. get a human readable representation of the sizes

The way those processing steps are combined is really similiar to what you do in haskell when composing functions. So it's actually not too far fetched to do some similar kind of scripting using haskell.  
The [Glasgow Haskell Compiler](http://www.haskell.org/ghc/) provides for a very nice way to execute little snippets of haskell code on the fly: the [haskell evaluation mode](http://www.haskell.org/ghc/docs/latest/html/users_guide/modes.html#eval-mode).

On a couple of occasions now I found myself in a situation where I wanted to use haskell to process some text, and do so quickly. Using the evaluation mode was nice but had some drawbacks for me:

* I had to address all functions with their respective namespace, e.g. `Data.List.sort`
* I didn't know how to add some custom utilities to allow for a more concise syntax

After posting a [question on stackoverflow](http://stackoverflow.com/questions/7888632/expression-evaluation-mode-in-haskell-for-scripting) I received some great feedback that helped me solve my issues. The example I used was this:

> "Find out all the folders that contained git diffs between the HEAD and a specific revision"

And the initial solution I used looked like this:

<pre class="terminal">
git diff --stat 9e2b68 | ghc -e \
  "getContents >>= return.(Data.List.nub).map(fst.break('/'==).head.words).lines" 
</pre>

## Enter the `ghci-dot-file`

It turns out both of the caveats can easily be resolved using a ghci-dot-file. A general description can be found [in the ghc docs](http://www.haskell.org/ghc/docs/latest/html/users_guide/ghci-dot-files.html).  
A minimal `.ghci` file might just predefine some qualified module imports. But the ghci-dot-files also allow to define some utilities, so mine now looks more like this:

~~~ {.haskell}
:set -w -fwarn-unused-binds -fwarn-unused-imports
import Text.Regex
import qualified Data.List as L
import qualified Data.Set as S
import Data.List.Split
import Maybe(isJust)
import Control.Arrow ((&&&))
import Data.Function(on)
import System.Directory
import Control.Monad(filterM)

let script f = getContents >>= return . f
let (=~) inp pat = isJust $ matchRegex (mkRegex pat) inp 
let uniq xs = S.toList $ S.fromList xs
let eachLine f = unlines . map f . lines
:set prompt "List,Set,Split,Regex> "
~~~

Turns out this makes it much nicer to use `ghc -e`, my initial example now looks like this:

<pre class="terminal">
git diff --stat 9e2b68 | ghc -e \
  "script $ L.nub . map(fst.break('/'==).head.words) . lines" 
</pre>

This version does actually not work properly...since it also lists files and not only folders. But that's easy to fix:

<pre class="terminal">
git diff --stat 9e2b68 | ghc -e \
   "getContents >>= filterM doesDirectoryExist . L.nub . map(fst.break('/'==).head.words) . lines" 
</pre>  
  

## Some scripting examples

Having set up the `.ghci` file here are some more examples of how I now use it, especially together with git.

### git committers in a project

This is a very simple example of using ghc to process some command output: Combined with a `git` command, it will list all committers of a project. The haskell part is still pretty modest:

~~~ {.haskell}
uniq . lines
~~~

Using this little script on Jasper Van der Jeugt's [blazeHtml](http://github.com/jaspervdj/blaze-html.git) repository will result in s.th. like this:
<pre class="terminal">
blazeJasper.git(master) > git log --format='%aN' | ghc -e 'script $ uniq . lines'
["Alex Mason","Chris Done","Frederick Ross","Harald","James Whitehead II",
"Jasper Van der Jeugt","Joeri Samson","Michael Snoyman","Pieter De Baets",
"Sergei Trofimovich","Simon Meier","Tom Harper","Yair Chuchem","oliver","zeuxis"]
</pre>

Not a lot of haskell functionality needed for that one. Let's look at a more involved example, where we want to find out not only the committers but also rank them according to the number of commits they contributed.  

### ranking committers in git

To do that we can

1. split the commit log into lines
2. filter out all lines that list an `Author`
3. sort and group the results so we know how many commits we have per author
4. count the commits and cut out only the email
5. sort our items according to the commit count
6. and print them out in reverse order

This can pretty much be transformed into the following haskell code:

~~~ {.haskell}
reverse .                          -- 6
L.sortBy(compare `on` snd) .       -- 5
map (last.words.head &&& length) . -- 4
L.group . L.sort .                 -- 3
filter((flip(=~)) "^Author") .     -- 2
lines                              -- 1
~~~

Running this as a haskell script through `ghc -e` get's us what we want:

<pre class="terminal">
blazeJasper.git(master) > git log | ghc -e \
  'script $ reverse.L.sortBy(compare `on` snd).map (last.words.head &&& length) . L.group.L.sort.filter((flip(=~)) "^Author").lines'
[("&lt;jaspervdj@gmail.com&gt;",457),("&lt;iridcode@gmail.com&gt;",91),
("&lt;jnwhiteh@gmail.com&gt;",25),("&lt;github.20.madhadron@spamgourmet.com&gt;",20),
("&lt;chrisdone@chris-dones-macbook-pro.local&gt;",13),("&lt;harald@zeuxis.de&gt;",10),
("&lt;oliver.mueller@gmail.com&gt;",9),("&lt;rtharper@rtharper-laptop.local&gt;",6),
("&lt;m@jaspervdj.be&gt;",5),("&lt;joeri@xaop.com&gt;",2),("&lt;yairc@tiny.local&gt;",1),
("&lt;slyfox@gentoo.org&gt;",1),("&lt;pieter.debaets@gmail.com&gt;",1),
("&lt;michael@snoyman.com&gt;",1),("&lt;harald@harald-linux.(none)&gt;",1),
("&lt;done@cn-done.(none)&gt;",1),("&lt;axman6@gmail.com&gt;",1)]
</pre>

## Adding IO actions in the mix

Sometimes you also need your script to perform further IO actions, say, append s.th. to a file.

### Tell git to ignore all *.txt files

An example might be the reoccurring situation that you created some local changes in my working directory and now want to tell git to ignore all *.txt files that are not yet added to the index.

<pre class="terminal">
test(master) > git status
# On branch master
# Untracked files:
#   (use "git add <file>..." to include in what will be committed)
#
#	.gitignore
#	foo/
#	me.txt
#	readme.txt

</pre>

Again this can easily be accomplished with a little haskell script, this time with the IO action in place.  
To start out we need to tell git to hand us a list of every element that is currently not administered by git. Next steps:

1. split all elements (each entry is on it's on line)
2. filter all *.txt files using a regular expression
3. put the result together
4. append this to the `.gitignore` file

Taking the output from git we can process the rest with haskell:

~~~ {.haskell}
(appendFile ".gitignore") .  -- 4
unlines .                    -- 3
filter((flip(=~)) ".*txt") . -- 2
lines                        -- 1
~~~

Try it out on my project:

<pre class="terminal">
test(master) > git ls-files --others --exclude-standard --directory --no-empty-directory | ghc -e \
  'getContents >>= (appendFile ".gitignore") . unlines . filter((flip(=~)) ".*txt") . lines'
test(master) > git status
# On branch master
# Untracked files:
#   (use "git add <file>..." to include in what will be committed)
#
#	.gitignore
#	foo/
</pre>

## More about ghci-dot-files

([This blog post](http://neilmitchell.blogspot.com/2010/01/using-ghci-files-to-run-projects.html) by Neil Mitchell has some nice examples of what you can use .ghci files for as well.)

[Starting with ghc version 7.4.1](http://hackage.haskell.org/trac/ghc/ticket/5265) it will become possible to specify additional .ghci files. This will allow for a much richer set of imports and definitions for the scripting usecase without polluting the usual ghci setup.

