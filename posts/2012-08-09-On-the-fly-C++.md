---
title: On-The-Fly C++
description: Using cling to interpret C++ code
tags: C++, llvm, clang
---

![](/images/cling/flying.jpg)

I can't recall how many times I had to write a basic small C or C++ program just to play around with an idea, the syntax of C++11 or anything similar. Very often indeed. Even though a good editor makes this very easy, it's kind of a burden to have to create a project directory, a source file, spell out the same old includes and the main function before you actually can start the task you where about to try.    
Then you compile and link, probably missing some libraries the first time until you __finally__ get to take it on a first test run.  

<div class="information rightinfo">

### Building Cling

You gotta checkout [llvm and clang](http://clang.llvm.org/get_started.html) from svn (do **not** use the git mirror! The makefile for cling uses the svn information!). Next checkout cling from svn into the tools folder and apply all the patches in the cling/patches directory and start your *configure - make - make install* cycle the same way you'd do for llvm and clang alone. Don't forget to include the  --enable-targets=host when running configure.
</section>

So why put up with all of this when the next ruby or python or haskell interpreter is just a couple of keystrokes away: type `irb` or `ghci` and of you go. No includes, no compile, no linker. Just get to the meat of your idea and start experimenting. Still, would be nice to have this for C++ as well.  
Yesterday I discovered something truly awesome: [cling](http://root.cern.ch/drupal/content/cling), an interactive interpreter for the C++ language based on clang/llvm.  
Kind of a pain to get it set up correctly (they do not yet provide binaries for download) but once it's done the fun can begin.

## Enter the C++ interpreter: cling

Now that cling is build we can run it. Best to call it with C++11 support enabled so all of the C++11 features that [clang](http://clang.llvm.org/) provides should also work in cling:

<pre class="terminal">
$ cling  -Wc++11-extensions -std=c++11

****************** CLING ******************
* Type C++ code and press enter to run it *
*             Type .q to exit             *
*******************************************
<span id="prompt">[cling]</span>$ 
</pre>

Ready for a test run:

<pre class="terminal">
<span id="prompt">[cling]</span>$ #include &lt;iostream&gt;
<span id="prompt">[cling]</span>$ using namespace std;
<span id="prompt">[cling]</span>$ int a[] = {1,2,3};
<span id="prompt">[cling]</span>$ for (int& x: a){ x += 10; }
<span id="prompt">[cling]</span>$ for (int& x: a){ cout << x << ","; }
11,12,13,
</pre>

Doing calculations in C can yield some surprising results if the types are not correct. Checking small stuff in an interpreter can really help!  
Same thing for bit fiddling...always nice to see how it will play out.

<pre class="terminal">
<span id="prompt">[cling]</span>$ float r = 7/9;
<span id="prompt">[cling]</span>$ r
(float) 0.000000e+00
<span id="prompt">[cling]</span>$ r = (float)7/9;
<span id="prompt">[cling]</span>$ r
(float) 7.777778e-01
<span id="prompt">[cling]</span>$ (0b1 << 5) | 0x1
(int const) 33
</pre>

Let's use cling for more complicated math stuff. So we include the math header file:

<pre class="terminal">
<span id="prompt">[cling]</span>$ #include &lt;math&gt;
input_line_36:1:10: <span style="color: #cb4b15;">fatal error:</span> 'math' file not found
#include &lt;math&gt;
      ^
</pre>

Uuh...header file not found...at least the error message is nice. 

<pre class="terminal">
<span id="prompt">[cling]</span>$ #include &lt;cmath&gt;
<span id="prompt">[cling]</span>$ cos(7)
(double const) 7.539023e-01
</pre>

Ok...that works nicely. What about using some C++11 features? Let's try a lambda:

<pre class="terminal">
<span id="prompt">[cling]</span>$ #include &lt;iostream&gt;
<span id="prompt">[cling]</span>$ using namespace std;
<span id="prompt">[cling]</span>$ auto func = [] () { cout << "Hello world" << endl; };
<span id="prompt">[cling]</span>$ func
(class <lambda at input_line_6:2:14>) @0x7f7ad79b1021
<span id="prompt">[cling]</span>$ func()
Hello world
</pre>

Wow! Really impressive. Not only can you try out regular C++ bits and pieces, you can also fool around with C++11 features!  
It's even possible to load and access system libraries using the `.L` load instruction. Here is a brief example of how to load the libpthread and call one of it's functions (`pthread_self`) to retrieve the ID of the calling thread:

<pre class="terminal">
<span id="prompt">[cling]</span>$ .L libpthread
<span id="prompt">[cling]</span>$ #include &lt;pthread.h&gt;
<span id="prompt">[cling]</span>$ pthread_self()
(pthread_t const) 0x7fff7da43180
</pre>


## Summary of Metaprocessor commands

Cling understands some meta-commands that are not valid C++. Those commands are usefull to instruct cling to carry out certain administrative tasks, such as

* `.x test.cpp` -- load a file (if the file defines void test() it will be executed)
* `.L libname` -- load a libary or file
* `.x filename.cxx` -- loads filename and calls void filename() if defined
* `.I path` -- adds an include path
* `.printAST` -- shows the abstract syntax tree after each processed entity
* `.q` -- exit cling
* `.help` -- display a brief description

## Worth a try

I'd say cling is definitely worth the try. It's not perfect but can be a real timesaver when fooling around with C or C++ code. Especially for little bits and pieces you want to try out while working on a C/C++ codebase, it's much less distracting to fire up a cling interpreter than to setup an example project.

## Limitations

While using cling I discovered several things that were not perfect yet:

* templates seem not to be supported
* the **auto** keyword does not seem to be implicit on declarations like: `i=5;`
* sometimes cling will segfault on small syntax errors and all current environment is lost

<pre class="terminal">
<span id="prompt">[cling]</span>$ std:string s("hi");
input_line_7:2:6: <span style="color: #cb4b15;">error:</span> unknown type name 'string'; did you mean 'std::string'?
 std:string s("hi");
     ^~~~~~
     <span style="color: #637204;">std::string</span>
/usr/include/c++/4.2.1/bits/stringfwd.h:59:33: note: 'std::string' declared here
  typedef basic_string<char>    string;
                                ^
Segmentation fault: 11
</pre>


## Further Information

Some interesting links people provided on [hacker news](http://news.ycombinator.com/item?id=4373334)

* [kind of an interactive C compiler](https://docs.google.com/viewer?url=http%3A%2F%2Fwww.bitsavers.org%2Fpdf%2Fsymbolics%2Fsoftware%2Fgenera_8%2FUser_s_Guide_to_Symbolics_C.pdf)
* [TCC, the *tiny* C compiler](http://bellard.org/tcc/)
* [google tech talk about cling](http://www.youtube.com/watch?v=f9Xfh8pv3Fs)
* [for fooling around: coderunner](http://krillapps.com/coderunner/)

<citation>Photo: NASA / Dryden Flight Research Center</citation>
