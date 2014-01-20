---
title: The Wonders of xargs
description: Exploring the goodness of xargs, one of the most versatile unix utilities.
tags: unix, bash
keywords: bash
---

[xargs](http://www.gnu.org/software/findutils/manual/html_node/find_html/Invoking-xargs.html) is a nice little utility for the command line that I use quite a lot. The basic usecases are pretty simple and it might even be so simple that the wonders of xargs go unnoticed for quite some time.  
_xargs_ is useful whenever you want to apply a command to a list of items in the shell. One really simple example:

<pre class="terminal">
$ find . -type f | xargs md5
</pre>

will calculate the md5 checksum of each file in the current directory:

<pre class="terminal">
MD5 (1.txt) = dfb5cfe29927bb659c380f553eb43131
MD5 (2.txt) = fd97581244bf2ff3241f977c432f2c0b
MD5 (3.txt) = 63a79bfa8ff94b772f16a6fea311b9a7
MD5 (4.txt) = 2e848e59c3a2abe9dd0fe250af7ac2dc
MD5 (5.txt) = 2098d6fce034058709de9120bd366bee
</pre>

Here _xargs_ will simply take the output of the `find` command and feed it to the `md5` command.  
The output will be fed as one chunk so the md5 command will be invoked like this:

<pre class="terminal">
$ md5 1.txt 2.txt 3.txt 4.txt 5.txt
</pre>

If you want xargs to split up the string and feed it one by one to the md5 command, you would need to use the `-n` option:

<pre class="terminal">
$ find . -type f -print0 | xargs -0 -n1 md5
</pre>

Note that _xargs_ will take any string and by default interpret spaces, tabs and newlines as delimiters.

A little more complicated form shows how _xargs_ can take a command where the first parameter is already bound.

<pre class="terminal">
$ find . -name "*.h" | xargs grep assert
</pre>

Up to here it's pretty straight forward since we only had to supply the the last parameter to the command after xargs. But it's a little more involved when the parameter is not a the end, e.g. in a copy command:

<pre class="terminal">
$ mkdir tmp
$ find . -type f -name "*.txt" | xargs -I xxx cp xxx tmp
</pre>

This will find all files ending in ".txt" and copy them into a temporary directory.  
Here the `-I` option allows to define a replacement string for the following command. So each input is bound to the variable `xxx` and can then be used in the copy command.  
Note that this will not work for files that contain spaces! To make it work with such nasty filenames we have to use the `-0` option:

<pre class="terminal">
$ find . -type f -name "*.txt" -print0 | xargs -0 -I xxx cp xxx tmp
</pre>

One very useful option is `-p`. This will prompt the user before executing each command invocation which sometimes is what you want.

<pre class="terminal">
$ find old -type f -name "*.txt" -print0 | xargs -0 -n1 -p rm
</pre>
will find all textfiles in the `old` directory, feed each of these to xargs (using the `-n1` option) and remove them after prompting the user.

It's good to know about _xargs_ and how to use it... It definitely WILL come in handy in lot's of situations.  
One final example ('cause it's kind of a favorite of mine):

<pre class="terminal">
$ find . -type d -name '.svn' -print0 | xargs -0 rm -rdf
</pre>

Will delete all the @*#!%-.svn folders recursivly from the current directory! Gone and forever!  
Long live the git!

