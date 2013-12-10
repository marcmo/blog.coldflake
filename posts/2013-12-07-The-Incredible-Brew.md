---
title: The Incredible Brew
description: The missing package manager for OS X
tags: unix, osx, tools, git, ruby, bash
---

![](/images/brew/flask.png)

When Apple started shipping the first iBooks including Mac OS X back in 2001 I was hooked. A beautiful and well design user interface sitting on top of a developer friendly operation system. More importantly: a *unix* based OS where most of the unix tools you know and love are available. But porting some linux based utility to Mac Os was now possible albeit quite hairy in some cases. Dealing with the dependencies and taking care that versions are compatible gets ungainly pretty quick.

## Package Management Systems

And of course there where others that stepped up and created whole package management systems: [fink] and [MacPorts] (formerly DarwinPorts) where the first contenders. While this ameliorated the whole situation considerably I still couldn't quite warm up to those solutions. Both systems decided it was best to not rely on any library that come with OS X but rather include everything in the dependency-tree. That for sure does guarantee the best possible compatibility but it also means build times will increase dramatically. MacPorts was long my favorite solution and over time I assembled quite a fair share of compiled libraries. So much that you will avoid rebuilding at all costs. If for some reason your dependencies are messed up and you'd like to restart...you won't do it.

## Homebrew to the Rescue

[Homebrew] takes a radically different approach: reuse existing Mac OS libraries and stay as lightweight as possible. Everything about homebrew has a modern feel to it, from the homepage over the way they keep all package information in a git-repository to the super easy formula (that's what they call their packages) definitions in ruby. It has a *very* active user community and since development is done on [github], it's quite easy to follow along and participate.  
The usual stuff works as expected:

* `brew update` -- update the formulae and Homebrew itself
* `brew outdated` -- now find out what is outdated
* `brew upgrade` -- Upgrade everything
* `brew upgrade $FORMULA` -- upgrade a specific formula
* `brew search /.*tk$/` -- search for available formula
* `brew info $FORMULA` -- provides a little more info about a formula

<pre class="terminal">
<span class="prompt">tmp</span> > brew info aubio
    aubio: stable 0.3.2
    http://aubio.org/
    Not installed
    From: https://github.com/mxcl/homebrew/commits/master/Library/Formula/aubio.rb
    ==> Dependencies
    Build: pkg-config ✔, libtool ✔, swig ✘
    Required: fftw ✘, libsamplerate ✘, libsndfile ✘
    ...
</pre>

The `search` command can be used with regular expressions if the search term is surrounded with slashes. A lesser known brew way to search for packages/formulae is to use `brew server`. This requires that [sinatra] is available on the system and provides a website for the search.

![](/images/brew/homebrew.png)

Of course you can always look up a package online as e.g. on [braumeister]. And if you need a package that is not yet part of homebrew, it can easily be created. All it takes is to locate the sources on the web and call `brew create` with this link. homebrew will create a basic formula (== a ruby script) that you can either just use or tweak manually of some special configure options are needed. Since all formulae are kept in a git repository, it's no problem to create your own packages and still keep up to date with homebrew.

## Warm and Fuzzy

Good package managing systems have one area that is crucial in the long run: help to maintain a tidy system. An area where brew really shines is dependency tracking. To find out if it's gonna be a quick install or quite a beast just take a look at the dependencies a formula has:

<pre class="terminal">
<span class="prompt">tmp</span> > brew deps cairo --tree
cairo
|- xz
|- pkg-config
|- freetype
|  |- libpng
|- fontconfig
|  |- pkg-config
|  |- freetype
|  |  |- libpng
...
</pre>

To maintain a sleek and tidy brew installation you can easily get rid of installed formulae. Here it's quite handy to do a reverse dependency lookup before:

<pre class="terminal">
<span class="prompt">tmp</span> > brew uses --recursive readline
abcl			geocouch		...
abook			ginac			...
...
</pre>

Once you decide what to get rid of, it just takes a `brew uninstall` or a `brew rm`. To make sure nothing unimportant remains in your system, `brew prune` will remove dead symlinks and `brew cleanup -s` will remove older versions and all downloads.  
Brew also comes with some extremely useful checks. The `brew doctor` makes sure you are in good shape and gives you good advice.

<pre class="terminal">
<span class="prompt">tmp</span> > brew doctor
Warning: Your XQuartz (2.7.4) is outdated
Please install XQuartz 2.7.5:
  https://xquartz.macosforge.org

Warning: Some installed formula are missing dependencies.
You should `brew install` the missing dependencies:

    brew install libffi

Run `brew missing` for more details.
...
</pre>

It tremendously helps to clean up and allows you to keep your peace of conscience. And `brew missing` of course shows you all formulae you still need to install.

## Absolutely Happy

When I found out about brew a couple of years back, I was thrilled by it's simplicity and the basic concepts behind brew. Today after having used brew on multiple OS versions and machines, brew is one of the first things to install. So far it never failed me and watching brew install a formula including all necessary dependencies is almost a calming experience. Looking forward to a lot of future installs!

![](/images/brew/cheers.png)

<citation>image: "Erlenmeyer Flask" by [Chris Kerr], "Cheers" designed by [Ryan Beck], both from the [Noun Project]  [(creativecommons)]</citation>


[fink]:http://fink.thetis.ig42.org
[macports]:http://www.macports.org
[homebrew]:http://brew.sh
[github]:http://github.com/mxcl/homebrew
[(creativecommons)]:http://creativecommons.org/publicdomain/zero/1.0/deed.en
[sinatra]:http://www.sinatrarb.com/
[braumeister]:http://braumeister.org/
[Chris Kerr]:http://thenounproject.com/chrisk3rr
[Ryan Beck]:http://thenounproject.com/RyanBeck
[Noun Project]:http://www.thenounproject.com/

