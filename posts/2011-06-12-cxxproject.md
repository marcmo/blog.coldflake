---
title: Building C/C++ Projects with Cxxproject
description: intro for rake based build tool for C/C++ projects
tags: C++, ruby, rake, dsl
---

![](/images/cxxproject/cxx_buildingblocks3.png)

Why are IDE's like [eclipse](http://www.eclipse.org) or Visual Studio so popular with many software developers? Yes, they offer an abundance of features and support to make it bearable to program in even the most verbose languages like Java and C++. Ok, the abundance of add-ons and plugins does allow for a nice integration of additional features and customization of the work environment.

Another advantage for many is the ability to have managed projects. This will allow you to just create a project (let's take C++ as an example) and will pretty much take care of setting up a build environment. Easy.

But this approach has some inherent drawbacks that might not be apparent at first:

* you are restricted to the options that were invisioned by the IDE/plugin creators
* you cannot change the internal build details (build succession, priorities etc.)
* you might have to deal with impenetrable xml (or other "binary") configuration files
* you are stuck with the toolchain your IDE offers (and thus probably with your IDE)

Of course in most cases you are not bound to stick with this *one-size-fits-all* approach. Often you can use a custom makefile to describe your build.
This is by far the more flexible solution but requires you to maintain makefiles... . But makefiles are so general it might turn out to be quite challenging for non-make-gurus to setup and maintain a well structured make-based build-system.

About a year ago a [friend of mine](http://gizmomogwai.tumblr.com) was working on a quite large C++ project that was built using Visual Studio. Since he is mostly working Mac and Linux he wanted to port the build system to s.th. less platform-dependent. That turned out to be quite a challenge and he ended up with a collection of rakefiles that he could now use on every platform.

[Rake](http://rake.rubyforge.org) is the ruby version of make and once you tried it you will love it. Since I was doing a lot of rake at the time and also quite often wrote rakefiles for my C/C++ projects we joined in the quest to find a suitable solution for this reoccurring task.
This was our wishlist for such a build system:

* command-line tool
* platform independent
* easy to set up and maintain a build-configuration for C/C++ projects
* support for scripting
* fast builds
* tool for programmers

We created an initial version of *[a build-system targeted towards C/C++ projects](http://marcmo.github.com/cxxproject/index.html)*. The result was quite usable but not very polished. It was based on rake and used ruby as a scripting language.
...and it worked quite well! I ended up doing all my C++ projects with this and even bigger projects where quite easy to set up.

The basic idea is that you have *building blocks* that depend on each other. Take an executable as an example: in order to build an executable, you might need several libraries the executable links to.

![](/images/cxxproject/cxx_buildingblocks1.png)

The executable itself needs to be modeled as a building block that depends on several library-building blocks. Of course this view is rather simplistic since the executable will also depend on some source files and header files that themselves will depend on header files a.s.o. In order to describe a build scenario those dependencies need to be captured so that they can be transformed into a precise succession of build instructions.

~~~ {.ruby}
exe "basic",
  :dependencies => ['libA', 'libB', 'libC']
~~~

You kind a get the idea. This is a sample of what a building block description looks like...and it's vanilla ruby code. Valid ruby code can easily be included. Say we want to add all source files that reside in any subdirectory to our building block description. A `FileList` is just what we can use here:

~~~ {.ruby}
exe "basic",
  :sources => FileList['**/*.cpp'],
  :dependencies => ['libA', 'libB', 'libC']
~~~

Other building blocks can be described in a similar manner. Check out the [tutorial](http://marcmo.github.com/cxxproject/docs/tutorial.html) to see some more complete examples.

Recently the project gained a lot of speed and we got some great new committers. We thought about how to improve our build-system at work (we are using eclipse and CDT). At first it was more of a case-study of what could be done with the cxxproject approach.  
After some experiments and some tuning (introducing multi-threaded builds a la `make -j`) we where able to build even huge projects (we are talking about building and linking more than 100 libraries) in a very efficient manner.

Working with this much bigger use-case we introduced a lot of little but nice improvements, e.g.:

* optional multi-threaded builds
* command line progress bar
* live-visualization of the build graph
* support for including make-files as steps
* support for custom building blocks

Currently we only support 3 different compiler-toolchains but more will follow:

* gcc
* clang
* diab

More features are on our road map. But the goal will always be to maintain a very **simplistic** system to configure C/C++ builds and do that well. We will soon see how this plays out when we get some more experience on bigger projects.
 

