---
title: Encouraging performance results
description: first results after introducing cxxproject on a large scale
keywords: C++, programming, productivity
tags: C++, ruby, rake, performance, dsl
---

It has been a while since we started to implement [cxxproject](https://github.com/marcmo/cxxproject) (early 2010) but finally we managed to adopt is as the underlying buildsystem throughout the largest projects the company I work for.  
I know, I know: It is foolish to build your own build system! But it's quite a learning experience and can have it's own [merits]. And as we now see can pay off!  

<section class="information">

### eclipse and CDT

The [CDT](http://eclipse.org/cdt/) (C/C++ Development Tooling) is easily the best available open source C++ IDE available. It is well suited for companies that need a feature rich environment while at the same time want to shape and customize it for individual use cases. Thus we were able to add support for our compiler or, as just recently, completely replace the make based CDT build environment with our custom build system.
</section>

Just to reiterate briefly: we were frustrated by a lot of aspects of the build-system that is normally used by the CDT: 

* the configuration files are an unreadable xml mess
* they are changed automatically on various dubious occasions so that you always get outgoing changes even if nothing should have been changed
* they are *impossible* to be merged!
* makefiles are generated on the fly, even if nothing needs to be built, which does take a long time!
* integrating pre- and post-steps is possible but tedious and does not consider build-dependencies correctly
* incompatibilities between different CDT versions
* ...

Cxxproject was the foundation that solved all of the above issues. The only problem: it's a build tool that targets programmers. You need to hack your configuration files as an embedded ruby DSL. This approach is extremely powerful but it's a long way from the cozy comfort of an IDE that seemingly takes care of all such aspects in the background.  
We now have a full integration into eclipse which provides all the IDE goodies while directly using cxxproject under the hood. Unfortunately this integration is currently not open sourced but we are working on it.  
Anyway...with the full IDE integration and an automatic way to convert CDT-style projects we had everything that was necessary to make the switch. Rather unexpectedly this switch was pretty smooth. And now after the first few weeks with the new build system we also gathered some interesting performance results.  
In the following charts we compared some daily build scenarios we have in one of our current projects. The project is a rather large software development where we build and link 207 different static libraries.  
The tests where conducted on a typical developer machine:

[merits]: /posts/2011-06-12-cxxproject.html

* Intel Xenon (4x 2.67GHz, 4GB RAM, Windows XP, HDD Raid)
* eclipse 3.6 with CDT 7.0.1
* Ruby 1.9.2

### Build whole workspace

Starting out with a fresh source code checkout and building all libraries and linking the final executable file turns out to be 39% faster.

![](/images/cxxproject_results/buildWorkspace.png)

### Rebuild workspace

Also interesting to compare is the speed for a rebuild. This is actually the most frequent use-case: change only a couple of files and want to rebuild and link the final executable.
Building the same workspace again without a clean in between was 73% faster!

![](/images/cxxproject_results/rebuildWorkspace.png)

### Clean everything

Cleaning everything should be very fast. Performing a clean for all 207 projects we previously built is now 78% faster.

![](/images/cxxproject_results/cleanEverything.png)

### Memory Consumption

Memory consumption was also interesting to see: While the build in eclipse swelled the amount of used memory by roughly 300 MB (and stayed there!!), the cxxproject build peaked at 35 MB and released all of it after the build.
That means we use 88% less memory then a regular CDT build. Awesome!

![](/images/cxxproject_results/memoryConsumption.png)

We will for sure continue to tweak our build system. But the first real numbers we collected are really promising! Always nice to see an idea pays of even when it might have looked foolish at the beginning.

