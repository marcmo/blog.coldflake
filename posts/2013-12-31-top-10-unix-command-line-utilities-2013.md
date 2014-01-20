---
title: Top 10 Unix Command Line Utilities 2013
description: Again a selection of the some useful unix commands.
tags: unix, bash
---

<div class="tabelofcontents rightinfo">

### Content

* [awk](#awk)
* [ls](#ls)
* [find](#find)
* [ack](#ack)
* [rename](#rename)
* [Redirects](#redirects)
* [Subshells](#subshells)
* [tee](#tee)
* [Zips & Archives](#zips-archives)
* [Base Conversion](#base-conversion)

</div>

For a long time I have used the bash on a daily basis. It has become such a trustworthy companion that I virtually cannot do any work on a windows box anymore. But even using bash every day I regularly come across ways of solving particular tasks that I did not really know about.  
To keep track of such solutions, I try to keep my cheat-sheets up to date with what I found and learned during the year. In tradition with [2011] and [2012] I again took a look at the git-log and compiled 10 of the things that I learned or used a lot in the shell.  
This year is different though since I decided to go into a little more detail about some of the items where I felt it was instructional. I also included a small table of contents, mainly 'cause I tend to come back to those tips myself a lot and wanted a quicker way to find the item in question.

## 1> awk

`awk` is probably the most powerful unix tool I encountered so far. It's actually a whole programming language, not just a tool. I tend to stay clear of the more complicated uses but for some small output processing it's just invaluable.  
The basic structure of `awk` statements is always the same, first you list a pattern to match, second the action you want `awk` to perform:

~~~ {.bash}
awk [condition] [{action}]
~~~

In the `action` it is possible to refer to different parts of the current input record. If `awk` is fed some lines of text, it will process each line that satisfy the condition and dissect it into parts. That is often helpful if only some parts of the output are wanted.  

### Dependent include files

One of the situations where this was just the right tool for me was trying to find recursively all dependent header files of a C-file. The compiler (*clang* in my case) can greatly help here since it has to know about the headers it needs. And it comes with an option to list this information:

~~~ {.bash}
-H                      Show header includes and nesting depth
~~~

I found this option is less picky than `-M`. Using it, the information about included header files will be displayed along with lots of other stuff that is unwanted in this case. `awk` can be used to filter the results down to the relevant lines and further extract the file names from those lines.  
In the example below

* all error messages are filtered out (`2>/dev/null`, see [redirects section](#redirects))
* clang is used to list the recursive include tree (`cpp -H [include-paths] [C/C++-file]`)
* `awk` first filters all lines with header information (`/^#.*\.h\"$/`)
* and extracts the header name (`{print $3}`)
* `tr` is used to get rid of the `""` quotation-marks
* finally all duplicates are removed by using `sort` and `uniq`

<pre class="terminal">
<span class="prompt">kernel</span>(master) > 2>/dev/null cpp -H -Iinclude crypto/fcrypt.c \
  | <span style="background-color:#C1DDFF; color:black">awk '/^#.*\.h\"$/ {print $3}'</span> | tr -d '""' | sort | uniq
include/asm-generic/atomic-long.h
include/linux/atomic.h
include/linux/bitmap.h
include/linux/compiler.h
include/linux/completion.h
include/linux/cpumask.h
include/linux/crypto.h
...
</pre>

## 2> ls

Plain old `ls` is probably my most used command. But `ls` can do some nice tricks, too. Using the `-S` flag, `ls` will sort the files by size. Combined with `-h` it produces a very readable result:

<pre class="terminal">
<span class="prompt">kernel</span>(master) > ls -lSh | head -5
total 6352
-rw-r--r--  1 muellero  staff   236K Jun  6  2013 sched.c
-rw-r--r--  1 muellero  staff   139K Jun  6  2013 cgroup.c
-rw-r--r--  1 muellero  staff   130K Jun  6  2013 sched_fair.c
-rw-r--r--  1 muellero  staff   104K Jun  6  2013 workqueue.c
</pre>

Often very helpful is also to only include the most recent files. `-t` will sort files by the date they were modified the last time.

~~~ {.bash}
ls -l -t
~~~

In a directory with lots of files it's sometimes practical to only list the directories. The `-d` option makes sure directories are listed as plain files and not searched recursively. Since all directories end in `/` this can be used to list only those.

~~~ {.bash}
ls -d */
~~~

Of course the opposite is also achievable, this one only lists the files. For this you can use `-p` which will write a slash ("/") after each filename if that file is a directory.

~~~ {.bash}
ls -p | ack -v /
~~~

This one can nicely be combined with some `awk` magic to count the file-types by extensions in a directory:

<pre class="terminal">
<span class="prompt">kernel</span>(master) > ls -p | ack -v / | awk -F . '{print $NF}' | sort | uniq -c
   1 Makefile
 112 c
   1 freezer
  17 h
  ...
</pre>

Here `awk` was used to split at the "." and print the last part (which is the extension).

## 3> find

Again a classic. After `ls` for sure an all-time favorite. Can also be combined with an action.  
Here, `find` first seeks out all symbolic links in the current directory and deletes them.

~~~ {.bash}
find . -type l -delete
~~~

This only works for reasonably recent versions of `find`. For older versions the following syntax should works as well:

~~~ {.bash}
find . -type l -exec rm {} \;
~~~

## 4> ack

Ever since I found [ack-grep] I most of the time us it as a replacement for `grep`. The developers claim that it is *"...designed as a replacement for 99% of the uses of grep..."* but to be honest, I never found that a case where it failed me. Out of the box it just works and produces beautifully highlighted results. But more importantly, it adds some nice features that I really grew to like.  
And it's quite fast. For most of my usecases faster then `grep` since it only searches whitelisted files by default. [This guy here] has put together a pretty good comparison with some performance tests.  

### Limit to certain File-Types

One very handy feature is the ability to narrow the search down to certain file types. By default `ack` already knows about countless file types (can be checked with `ack --help=types`). Limiting the searched files dramatically reduces the search time.  
Here is an example of searching all C and C++ source files for a pattern and include 2 lines of context before (`-B`) and after (`-A`) the found match.

<pre class="terminal">
<span class="prompt">kernel</span>(master) > ack --type=cpp probability -B 2 -A 2
<span style="color:#60FA67">drivers/net/ethernet/sun/cassini.h</span>
<span style="color:#FFFC67">875</span>-
<span style="color:#FFFC67">876</span>-/* probabilities for random early drop (RED) thresholds on a FIFO threshold
<span style="color:#FFFC67">877</span>: * basis. <span style="background-color:#FFFC67; color:black">probability</span> should increase when the FIFO level increases. control
<span style="color:#FFFC67">878</span>: * packets are never dropped and not counted in stats. <span style="background-color:#FFFC67; color:black">probability</span> programmed
<span style="color:#FFFC67">879</span>- * on a 12.5% granularity. e.g., 0x1 = 1/8 packets dropped.
<span style="color:#FFFC67">880</span>- * DEFAULT: 0x00000000
</pre>

### Highlighter

A lesser common usecase for `ack` is the *passthru* mode. In this mode `ack` does not limit the output to the matching positions but spills out the whole input, highlighting the search-matches in the process. This practically makes `ack` a formidable highlighting tool.

<pre class="terminal">
<span class="prompt">kernel</span>(master) > ls | ack -i mutex --passthru
...
lockdep_internals.h
lockdep_proc.c
lockdep_states.h
module.c
<span style="background-color:#FFFC67; color:black">mutex</span>-debug.c
<span style="background-color:#FFFC67; color:black">mutex</span>-debug.h
<span style="background-color:#FFFC67; color:black">mutex</span>.c
<span style="background-color:#FFFC67; color:black">mutex</span>.h
notifier.c
nsproxy.c
...
</pre>


## 5> rename

Mass renaming in the shell is usually done with some form of loop, e.g.

~~~ {.bash}
for i in *.zip ; do mv "$i" "${i%.zip}`date +%Y`.zip"; done
~~~

Here a substitution operation is used to cut of the extension part which is then replaced with the year + the extension. The same thing using `rename` would look like this:

~~~ {.bash}
rename -X -a `date +%Y` *.zip
~~~

This is using the `-a` transform that appends some string to each filename.

### rename part of filename

Using for loops can become complicated pretty soon, and for those cases `rename` is a pretty good alternative. Say when you have a bunch of files that contain the string "Aug" in their name but want to replace it with "08". The by far easiest way I have found to accomplish stuff like this is to use the `rename` command.

~~~ {.bash}
rename 's/Aug/08/' *.*
~~~

`rename` takes *modification rules* and applies them to the files that match a pattern (if given, otherwise it expects a list of filenames on `stdin`). The beauty of this utility is that it comes with support for lot's of common cases out of the box. For example, if files contain spaces or other unwanted characters, there is the option to sanitize them using `-z`.

<pre class="terminal">
<span class="prompt">tmp</span> > ls
a b  c_.tif
<span class="prompt">tmp</span> > rename -n -z *
'a b  c_.tif' would be renamed to 'a_b_c_.tif'
</pre>

Here the option `-n` is applied as well, resulting in a *dry run* without any actual modifications. The `-z` option will replace all sequences of whitespaces or control characters with a single "\_", replace every shell meta-character with "\_" and remove spaces and underscores from left and right end.  
The result is almost as desired&hellip; just the trailing "\_" is still annoying. The sanitize command did not remove it. That's because the file extension is the last part of the filename. But I'd really like to sanitize the name without the extension. Turns out `rename` has a very handy feature that allows to save and remove the last extension before any modifications and slab it on again afterwards.

<pre class="terminal">
<span class="prompt">tmp</span> > rename -n -X -z *
'a b  c_.tif' would be renamed to 'a_b_c.tif'
</pre>

### put files in folders according to their endings

This is something I use more and more often.

~~~ {.bash}
rename -p -X -e '$_ = "$EXT/$_" if @EXT' *
~~~

This will move all files into folders that have the same name as their extension. The `-p` is needed to make sure directories are created if needed. The `-X` chopes of the extension and saves it into the `$EXT` variable. `-e` will evaluate the following expression, in this case evaluate to a path consisting of the stored extension together with the filename if an extension exists.

* `p`    &#x2192; creates directories if needed
* `X`    &#x2192; chop of extension and append after the operation
* `e`    &#x2192; evaluate perl expression
* `$EXT` &#x2192; A string containing the accumulated extensions saved by "-X" switches, without a leading dot
* `@EXT` &#x2192; An array containing the accumulated extensions saved by "-X" switches, from right to left, without any dots

Note that `rename` is not available on all systems by default and you might need to install it, e.g. `brew install rename` on OSX.

## 6> Redirects

For some shell commands I always have to consult my notes or google. Mostly that is 'cause I haven't taken the time to really understand the underlying concept. Redirects are a perfect example: simple enough to just use them so most people don't worry about their mechanics.

<div class="information leftinfo">

### Everything in UNIX is a file

Data streams and peripherals are treated as *files*, just like ordinary files. Each gets a file-descriptor assigned that can then be used to access the stream.  
A *file-descriptor* is an integer associated with a network connection, a pipe or a real file, amongst other things. When executing a command, it will mainly work with 3 different file-descriptors/files:

* 0 &#x2258; stdin
* 1 &#x2258; stdout
* 2 &#x2258; stderr

</div>

### Redirecting Output

For redirecting output you use "**>**" (the output redirection operator).
Probably my most widely used redirect is to write to a file instead of `stdout`. The example will write to a .gitignore file, possibly creating it in the process if it doesn't already exist.

~~~ {.bash}
echo temp > .gitignore
~~~

Slightly more useful is often to use the same redirect but append to a file rather than to overwrite it:

~~~ {.bash}
echo temp >> .gitignore
~~~

Such basic redirects are valid for the whole line and are a short form for specifying the target file descriptor explicitly (`>>` &#x2261; `1>>`). They can also appear before the command:

~~~ {.bash}
1>> .gitignore echo temp # redirect stdout and append to .gitignore
~~~

![](/images/unixtools2013/Stdout2file.png)

Of course redirects are not limited to redirecting to a file. Have you ever tried to redirect the output of a command to a file but there were still some messages displayed on the terminal that did not get redirected? This is a quite common case where `stderr` needs to be redirected to `stdout` so that all error messages are sent to `stdout`:

~~~ {.bash}
myCommand 2>&1
~~~

![](/images/unixtools2013/RedirectStderr.png)

The **&** is similar to the address operator in C/C++ and is used to name a file-descriptor. So `2>&1` tells the shell that we want file-descriptor **(2)** (`stderr`) to point to the same file as file-descriptor **(1)** (`stdout`). This works with any file-descriptor, not just **(2)** and **(1)**: `x>&y` will point file-descriptor x to where y is pointing to.  
Using this knowledge, we can swap where 2 file-descritors point with a technique similar to pointer swapping in C:

~~~ {.bash}
myCommand 3>&1 1>&2 2>&3 3>&-
~~~

A temporary file-descriptor **(3)** is used as a temp. It will first capture the file pointed by `stdout` which is then be redirected to where `stderr` points to. As a last step `stderr` is now pointed to where `stdout` pointed originally and **(3)** is closed as it no longer is needed.

![](/images/unixtools2013/redirect_swap.png)

### Order of Redirects

A fact that often causes considerable confusion is that the order of the redirects matters. Actually not to hard to remember once you picture the file-descriptors as pointers to files. If we for example try to capture stdout and stderr in a file, one might be tempted to use something like this:

~~~ {.bash}
myCommand 2>&1 >> my_log_file.txt
~~~

![](/images/unixtools2013/WrongRedirect.png)

But this does not work as expected. Here, `stderr` is first pointed at the same file as `stdout` (i.e. the terminal). Then we redirect `stdout` away from the terminal to a file.  
If both `stdout` and `stderr` should be captured in a file, we need to reverse the order of the redirects:

~~~ {.bash}
myCommand >>my_log_file.txt 2>&1 # capture everything in a file
~~~

![](/images/unixtools2013/Stdout2file2.png)

Armed with an understanding how redirects work, it's now quite simple to understand the following example:

### Only use stderr output

Redirect `stderr` to where `stdout` points to and then `stdout` to /dev/null (dump it). The output can then further be processed, here we pipe it to grep for something.

~~~ {.bash}
myCommand 2>&1 >/dev/null | grep 'foo'
~~~

![](/images/unixtools2013/OnlyStderr.png)


### Redirecting Input

Similar to how we can modify `stdout` and `stderr`, `stdin` can also be replaced as an input to a program using the input redirection operator "**<**".

~~~ {.bash}
myCommand < inputFile # same as cat inputFile | myCommand
~~~

Combining input and output redirection is also possible for one command:

~~~ {.bash}
myCommand < inputFile > outFile
~~~

## 7> Subshells

Quite often I want to download some file using `wget` and put it into my download folder. At the same time I don't want to loose the context (current working directory).  
One way to deal with this is to `cd` into the download directory, issue the `wget` command, and use a quick `cd -` to return from where you started. Lately my preferred way is to use a *subshell*.

<pre class="terminal">
<span class="prompt">tmp</span> > (cd ~/downloads; wget http://www.example.com/abc.tar.gz)
</pre>

A subshell is a child process of the process the shell is running in with access to the environment of it's parent process. But any changes to the environment done in this child-process does not propagate to the parent. Thus we can change the directory, set or unset environment variables and so on.  
Another nice example is to remove the `http_proxy` from the environment when temporarily not needed:

<pre class="terminal">
<span class="prompt">tmp</span> > (unset http_proxy; wget http://www.example.com/abc.tar.gz)
</pre>

### Bonus Example

I just found another very nice usage of subshells on the discussion board of [vimcasts]. Barton Chittenden showed how to avoid the use of temporary files for using vimdiff using process substitution in bash:

<pre class="terminal">
<span class="prompt">tmp</span> > vimdiff <(sort file1) <(sort file2)
</pre>

## 8> Tee

Sometimes there is more then one consumer for some command output. This is what `tee` can be used for. It takes as an input the output of some other command using `stdin` and duplicates it, feeding the two streams to a file-descriptor and `stdout`.

![](/images/unixtools2013/tee.png)

After taking a look at [redirects](#redirects) and [subshells](#subshells) here is a neat [commandlinefu-example] of how `tee` can be used to split a pipe into multiple streams for one or more subshells to work it.

<pre class="terminal">
<span class="prompt">tmp</span> > echo "tee can split a pipe in two" | <span style="background-color:#C1DDFF; color:black">tee >(rev) >(tr ' ' '_')</span>
tee can split a pipe in two
owt ni epip a tilps nac eet
tee_can_split_a_pipe_in_two
</pre>

### Copy directory multiple time

To copy a directory containing everything without temporary files and preserving ownership, permissions, and timestamps is often done with `tar`.

~~~ {.bash}
tar cf - . | (cd targetDir && tar xfp -)
~~~

`tar` will create an archive containing everything in the current directory. The "-" is used to write to `stdout` instead of a file. The output is then piped into a [subshells](#subshells) to change into the target directory and extract the everything.  
Copying everything twice can be accomplished using `tee`:

~~~ {.bash}
tar cf - . | tee >(cd targetDir && tar xfp -) | (cd targetDir2 && tar xfp -)
~~~

Here `tee` will duplicate `stdin` and feed it to a process substitution and to `stdout`, which in turn is then piped to the subshell.

## 9> Zips & Archives

Compressed archives are a brilliant way to exchange files&hellip; everything is bundled together, in general takes up way less space and can even be password protected. Inspecting or unpacking them usually involves creating temporary directories or files. But there are some handy ways to acoid such intermediate products.

### view zipfiles

Often you just need to peek inside without actually extracting anything. `zipinfo` is a nice little utility that does just that.

<pre class="terminal">
<span class="prompt">tmp</span> > zipinfo tmp.zip
Archive:  tmp.zip   20400 bytes   2 files
-rw-r--r--  3.0 unx    19960 bx defN 14-Nov-12 11:05 colordiff-1.0.13.tar.gz
-rw-r--r--  3.0 unx       72 bx stor 14-Nov-12 11:06 colordiff-1.0.13.tar.gz.sig
2 files, 20032 bytes uncompressed, 20022 bytes compressed:  0.0%
</pre>

But of course unzip can also perform the task. The `-t` option tests an archive file, listing it's content in the process:

<pre class="terminal">
<span class="prompt">tmp</span> > unzip -t tmp.zip
Archive:  tmp.zip
    testing: colordiff-1.0.13.tar.gz   OK
    testing: colordiff-1.0.13.tar.gz.sig   OK
No errors detected in compressed data of tmp.zip.
</pre>

A third and my prefered option is to use `unzip` with it's `-l` option (lists the content of the archive file).

<pre class="terminal">
<span class="prompt">tmp</span> > unzip -l tmp.zip
Archive:  tmp.zip
  Length     Date   Time    Name
 --------    ----   ----    ----
    19960  11-14-12 11:05   colordiff-1.0.13.tar.gz
       72  11-14-12 11:06   colordiff-1.0.13.tar.gz.sig
 --------                   -------
    20032                   2 files
</pre>

### Unpacking

Even upacking does not necessarily involve spilling the zipped files out to disk. The content of a zip-archive can be extracted and fed to a pipe so that it becomes usable by another process.

<pre class="terminal">
<span class="prompt">tmp</span> > echo "text me" > test.txt
<span class="prompt">tmp</span> > zip storage test.txt; rm test.txt
  adding: test.txt (stored 0%)
<span class="prompt">tmp</span> > ls
storage.zip
<span class="prompt">tmp</span> > unzip -p storage.zip | rev
em txet
<span class="prompt">tmp</span> > ls
storage.zip
</pre>

Here I created a sample zip archive and extracted it to feed the content to `rev` without creating any files in the process.

### Inspecting tar Archives

Most of the archives dealt with under Linux are compressed tar files, so here is how you list their content.  
First, for a gzipped file (ending in `*.tar.gz` or `*.tgz`):

~~~ {.bash}
tar -ztvf file.tar.gz
~~~

And finally for the `bzip2` formats:

~~~ {.bash}
tar -jtvf file.tar.bz2
~~~

* `t` &#x2192; list contents
* `v` &#x2192; verbose, display detailed information
* `z` &#x2192; filter through gzip (for *.gz fils)
* `j` &#x2192; filter through bzip2 (for *.bz2 fils)
* `f` &#x2192; filename

## 10> Base Conversion

Even though `printf` in C/C++ offers great many pitfalls, I still like to use it often despite having access to C++ iostreams. Bash also supports a form of `printf` that can easily be used to do some basic number conversions.

### convert decimal to hex/octal

Just as in C, the bash `printf` can easily print numerical values in different formats:

<pre class="terminal">
<span class="prompt">tmp</span> > printf "%#x\n" 100
0x64
<span class="prompt">tmp</span> > printf "%#o\n" 100
0144
</pre>

The `#`-character is the *alternative format modifier* and is responsible for prepending the "0x" for hexadecimal values and a leading zero for octal values.

### convert hex to decimal

<pre class="terminal">
<span class="prompt">tmp</span> > printf "%d\n" 0x64
100
</pre>

### Print a conversion table from decimal to hex

A nice example I found in the [Bash Hackers Wiki] is to print out a conversion table:

<pre class="terminal">
<span class="prompt">tmp</span> > for ((x=0; x <= 127; x++)); do printf '%3d | 0x%02x\n' "$x" "$x"; done
  0 | 0x00
  1 | 0x01
  2 | 0x02
  3 | 0x03
  ...
</pre>

## Done for 2013

Ok, this was my bash feature list of the year. By no means am I an expert for the features described. If you see something that is incorrect or might otherwise be completed more elegantly I'd be glad to hear it!

[2011]:/posts/2011-12-31-bash-features-of-the-year.html
[2012]:/posts/2012-12-30-Top-10-Unix-Command-Line-Utilities-2012.html
[Bash Hackers Wiki]:http://wiki.bash-hackers.org/commands/builtin/printf
[This guy here]:http://codeseekah.com/2012/03/11/ack-grep-vs-grep/
[ack-grep]:http://beyondgrep.com/
[commandlinefu-example]:http://www.commandlinefu.com/commands/view/6334/use-tee-to-process-a-pipe-with-two-or-more-processes
[vimcasts]:http://vimcasts.org/episodes/comparing-buffers-with-vimdiff/
