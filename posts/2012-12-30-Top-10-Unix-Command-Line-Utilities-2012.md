---
title: Top 10 Unix Command Line Utilities 2012
description: another selection of the some useful unix commands
tags: unix, bash
---

![](/images/unixtools2012/aurora.jpg)

This year has been quite busy with lot's of great but stressful changes in my professional and personal life. That's why I did not find a lot of time to write new blog posts. Now that the year is turning to an end I at least want to summarize some of the unix commands I found helpful over the year.  
As [last year](/posts/2011-12-31-bash-features-of-the-year.html) I'm going to list 10 unix commands out of a larger collection of little examples I jotted down. The list has no particular order, just the way they came in handy for me.

## 1> tr

Whenever you need to do some small text substitutions `tr` can come in handy (tr stands for *translate* or *transliterate*). It will take some input, apply a transformation and spit out the result.  
`tr` takes 2 parameters, the first one is a set of characters that it should translate, the second the set of characters that will act as a replacement. So the arguments **"abc"** **"123"** would mean that **a** gets replaced by **1**, **b** with **2** and so on.

As a simple example, this line changes the case of the characters 'a' through 'z':

<pre class="terminal">
<span id="prompt">tmp</span> > echo "Hello" | tr "A-Za-z" "a-zA-Z"
hELLO
</pre>

More realistic example: split your $PATH into it's elements:

<pre class="terminal">
<span id="prompt">tmp</span> > echo $PATH | tr ":" "\n" | sort

    /Users/oliver/.cabal/bin
    /Users/oliver/.rvm/bin
    /Users/oliver/.rvm/gems/ruby-1.9.3-p0/bin
    /Users/oliver/.rvm/gems/ruby-1.9.3-p0@global/bin
    /Users/oliver/.rvm/rubies/ruby-1.9.3-p0/bin
    /Users/oliver/local/node/bin
    /Volumes/macbox_cs/dev/android-sdk-macosx/platform-tools/
    ...
</pre>


## 2> sort

Simple command to sort input in different manners. By default this in alphabetic order, but using the `-n` option will sort in a numeric fashion:
<pre class="terminal">
<span id="prompt">tmp</span> > du /bin/* | sort -n -r | head -4
1320	/bin/ksh
1264	/bin/sh
1264	/bin/bash
592	/bin/zsh
</pre>

`sort` will take multiple files as input and will merge and sort all of the files for you. Some of the most used options include `-r` for sorting in reverse order and `-f` for sorting case-insensitive.

## 3> uniq

Want to get rid of duplicate lines? `uniq` solves this problem efficiently. Note that it will only compare adjacent lines for equality, so you might want to sort before you use `uniq`.  
Nice options: `-c` will prepend the count of equal elements before a line, `-u` will only output lines that are *not* repeated and `-i` does the whole thing case-insensitive.

Here is an example that combines **tr**, **sort** and **uniq** such that you can get the frequency of all words in a wikipedia article:

<pre class="terminal">
<span id="prompt">tmp</span> > curl http://en.wikipedia.org/wiki/Minimum_spanning_tree \
      | tr -cs "A-Za-z" "\n" | tr "A-Z" "a-z" \
      | sort | uniq -c | sort -n -r

% Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100 93342  100 93342    0     0   279k      0 --:--:-- --:--:-- --:--:--  323k
1031 a
 568 span
 442 href
 435 class
 308 li
 300 b
 284 title
 229 wiki
 211 the
 209 cite
 206 id
 192 spanning
 184 i
 169 tree
 166 minimum
 ...
</pre>

This fetches an html-page from wikipedia and first does some preprocessing using `tr`:  
`tr -cs "A-Za-z" "\n"` &mdash; split on all non-alphabetic characters  
`tr "A-Z" "a-z"` &mdash; make everything lowercase  
`sort | uniq -c` &mdash; sort, remove dups but remember the **c**ount  
`sort -n -r` &mdash; sort **n**umerically in **r**everse order

## 4> split and cat

Again a very simple command but can be surprisingly helpful.  
This is an example that splits a huge file into 75 MB chunks:

~~~ {.bash}
split -b 75m input.zip
~~~

This will result in a bunch of files that are named with 3 letters starting from `xaa`,`xab`,...  
To reassemble the lot, all those files have to be con**cat**inated in alphabetic order:

~~~ {.bash}
cat `ls x*` > reassembled.zip
~~~

Just a quick check to make sure we ended up with the same content:

<pre class="terminal">
<span id="prompt">tmp</span> > ls *.zip | xargs md5
MD5 (input.zip) = d760b448595f844b1162eaa3c04f83d8
MD5 (reassembled.zip) = d760b448595f844b1162eaa3c04f83d8
</pre>

## 5> substitution operations

Operations on multiple files are very frequent. Some situation I found myself in several times was that I needed to extract audio from a bunch of mp4 files.  
I [found] 2 good ways to solve this: my prefered one makes use of *substitution operations*:

~~~ {.bash}
for i in *.mp4; do ffmpeg -i "$i" "${i%.mp4}.mp3"; done
~~~

Here the subtitution operator `${i%.mp4}` deletes the shortest possible match from the right side.  
This is nice and terse...but there is another variant that might even be a little more explicit: using *basename*

~~~ {.bash}
for i in *.mp4; do ffmpeg -i "$i" "`basename $i .mp4`.mp3"; done
~~~

## 6> calculate the size of all files found by find

There are for sure hundreds of ways to achieve this...I liked the combination of a simple `find` with a short and sweet `awk` function:
<pre class="terminal">
<span id="prompt">tmp</span> > find . -iname "*.png" -ls | awk '{s += $7} END {print s}'
2076723
</pre>

As some people on [hn] pointed out *awk* is probably not the simplest solution for summing up space usage. So I include an example inspired from [this blog].
<pre class="terminal">
<span id="prompt">tmp</span> > find . -iname "*.png" -print0 | xargs -0 du -ch | tail -1
2.2M	total
</pre>

## 7> df

Classic. Collects some disk space usage information about your system.

<pre class="terminal">
<span id="prompt">tmp</span> > df -h
Filesystem     Size   Used  Avail Capacity  iused   ifree %iused  Mounted on
/dev/disk0s2  156Gi  138Gi   17Gi    89% 36247400 4528347   89%   /
...
</pre>

## 8> dd

Basically `dd` is just a form of copying from some input to some output (by default from stdin to stdout) that let's you configure the block size used for the copy. It will duplicate a bitstream from it's input. I've also heard  people call it **d**ata **d**estroyer 'cause you can easily shoot yourself in the foot by inadvertently mixing up input and output...  
Turns out there are quite some interesting usecases for it.

A nice one I found [here] is to securely wipe your drive: overwrite the entire drive with 0s:

~~~ {.bash}
dd if=/dev/zero of=/dev/hda
~~~

More secure (means harder to recover) is to use random data to wipe the drive:

~~~ {.bash}
dd if=/dev/urandom of=/dev/hda
~~~

And for the paranoid and the US Government we can repeatedly execute the fun:

~~~ {.bash}
for n in `seq 7`; do dd if=/dev/urandom of=/dev/sda bs=8b conv=notrunc; done
~~~

### Safe MBR

A less destructive example shows how to create an image of the entire master boot record (including the partition table):

<pre class="terminal">
<span id="prompt">tmp</span> > dd if=/dev/sda of=MBR.img bs=512 count=1
</pre>

Here *count=1* means copy only 1 input block, *bs=512* sets both input and output block size to 512 bytes.

### Generate Randomness

Sometimes very handy is to use `dd` to generate some random data for a file:

~~~ {.bash}
dd if=/dev/random of=random.bin bs=100 count=1
~~~

### Tracking Progress

In some instances the process started with dd will take a considerable amount of time. Since you will not get any fancy progress bars, there is some trick to find out about the progress.  
First you need to find out about the process id of the dd process:

<pre class="terminal">
<span id="prompt">tmp</span> > pgrep -l '^dd$'
4523 dd
</pre>

Then send the USR1 signal to the dd process:

<pre class="terminal">
<span id="prompt">tmp</span> > kill -USR1 4523
</pre>

When `dd` detects the USR1 signal, it will print out the current statistics to its stderr.

<pre class="terminal">
<span id="prompt">tmp</span> > 123122312 bytes (xxx GB) copied, 3965.94 s, 13.9 MB/s
</pre>

After reporting the status, dd will resume copying. To keep it going use [watch](/posts/2011-12-31-bash-features-of-the-year.html):

<pre class="terminal">
<span id="prompt">tmp</span> > watch -n 10 kill -USR1 4523
</pre>

## 9> zip

Even though I prefer tar with either gzip or bzip2, the *zip* format is widely used especially among windows users. So I frequently use `zip` and `unzip` as well. Since it works quite differently compared to tar, I list the main usecases I need:

Most simple case: add some files to a zip-file (called "abc.zip"):

~~~ {.bash}
zip abc file1 file2 file3
~~~

Of course you can also copy a whole directory "tmp" into "abc.zip".

~~~ {.bash}
zip -r abc tmp
~~~

Also quite handy: creating a password protected archives:

~~~ {.bash}
zip -e important.zip file1 file2
~~~

And finally list the files inside an archive

~~~ {.bash}
unzip -l a.zip
~~~

## 10> hexdump

When dealing with binary files it is often necessary to glimps a quick view to the actual data. I found that having a little command line utility can be very practical for such cases. `hexdump` has exactly what I need.

<pre class="terminal">
<span id="prompt">tmp</span> > hexdump  new.zip | head -5
0000000 70 a9 20 8d b1 a3 5c 1c 16 e3 17 b2 ef 94 16 ac
0000010 85 40 59 f9 89 40 45 ed 61 e8 10 f5 6f f5 99 a2
0000020 3a d6 69 62 e0 ab ee 0a 67 b8 c5 21 58 42 4d 52
0000030 2d 78 ae 2a 31 f2 78 c7 1f 22 99 07 e1 6a 55 bb
0000040 68 9a fe 8f c3 e0 e5 a3 4c 7d b3 6b f9 ae de 92
</pre>

You can instruct it to display also the corresponding ASCII representation:

<pre class="terminal">
<span id="prompt">tmp</span> > hexdump -C new.zip | head -5
00000000  70 a9 20 8d b1 a3 5c 1c  16 e3 17 b2 ef 94 16 ac  |p. ...\.........|
00000010  85 40 59 f9 89 40 45 ed  61 e8 10 f5 6f f5 99 a2  |.@Y..@E.a...o...|
00000020  3a d6 69 62 e0 ab ee 0a  67 b8 c5 21 58 42 4d 52  |:.ib....g..!XBMR|
00000030  2d 78 ae 2a 31 f2 78 c7  1f 22 99 07 e1 6a 55 bb  |-x.*1.x.."...jU.|
00000040  68 9a fe 8f c3 e0 e5 a3  4c 7d b3 6b f9 ae de 92  |h.......L}.k....|
</pre>

Combining hex and octal output quickly allows for relating the hex values to their octal counterparts:

<pre class="terminal">
<span id="prompt">tmp</span> > hexdump -xb new.zip | head -5
0000000    a970    8d20    a3b1    1c5c    e316    b217    94ef    ac16
0000000 160 251 040 215 261 243 134 034 026 343 027 262 357 224 026 254
0000010    4085    f959    4089    ed45    e861    f510    f56f    a299
0000010 205 100 131 371 211 100 105 355 141 350 020 365 157 365 231 242
0000020    d63a    6269    abe0    0aee    b867    21c5    4258    524d
</pre>


<citation>[Photo: David Mark/Pixbay]  [(creativecommons)]</citation>

[found]:http://www.debian-administration.org/articles/150
[Photo: David Mark/Pixbay]:http://pixabay.com/en/users/tpsdave/
[(creativecommons)]:http://creativecommons.org/publicdomain/zero/1.0/deed.en
[here]:http://www.marksanborn.net/howto/wiping-a-hard-drive-with-dd/
[hn]:http://news.ycombinator.com/item?id=4985393
[this blog]:http://mrnugget.github.com/blog/2012/10/24/command-line-ride/
