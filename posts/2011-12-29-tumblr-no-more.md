---
title: Good Bye Tumblr - Welcome Hakyll
description: moving away from mum-style blogging
keywords: blogging, programming, productivity
tags: blogging, git
---

This year I finally got around to start writing about some of the my technical projects and programming related tasks. And starting out fresh the quickest way was to use some ready-to-go blogging framework. The choices for this are actually pretty amazing...ranging from microblogging services like [Posterous] and [Tumblr] to full fledged blogging frameworks like [Wordpress] and [Blogger]. Of those services Tumblr seemed like an especially good fit for my needs.

<div class="information rightinfo">

### Why Tumblr

* Very quick to get something going
* You can use custom CSS elements
* Source code formatting was possible using [pygments]
* And very important: It allows you to use your own domain so when moving away from Tumblr you can keep the same URL for your blog

</div>

Despite all the merits such an easy to-go solution offers, I have to admit I always felt a little embarrassed as a programmer to use the same tool my grandma probably would (and *could*). This might have been bearable &#8230; but the thing that kind of tipped me over the edge was the constant annoyance with the html content that I would end up with Tumblr. Yes, it is possible to use custom html or even a mixture of html and [markdown], but unfortunately the html that you put into Tumblr is *not* the html that will eventually end up in your blog. That was very unfortunate since I initially tried to use [pandoc] formated source-code fragments. In Tumblr that would not work. Even though I found a practical solution using [pygments], not having full control over the output eventually became a no-go for me (ever tried to render graphs using javascript in Tumblr?).  
So it was clear: I needed a solution for my requirements:

* full control over the generated content
* including the usage of javascript and various javascript libraries
* easy to deploy
* *not* suitable for my grandma (well, what I mean is: a programmer tool)

But then again the variety of approaches that all might be quite viable for a blog make it hard to settle on one technology. So over the course of several month I have been toying with dynamically generated sites using [Rails] or [Express] (an extremely pleasant web framework based on [nodejs]), and the static generators [jekyll] and Jasper Van der Jeugt's wonderful [Hakyll] framework. Since I didn't really have a need to generate any content dynamically and also had no data I'd put into a database the more lightweight approach of the later two was fully sufficient.  

<div class="information leftinfo">

### Why Hakyll

* minimalistic configuration
* quick test-fix-test cycle
* power of a real programming language to extend it
* definitely a programmer tool!

</section>

Hakyll has a special appeal to me since it is kept pretty minimal and it is written in [my favorite programming language](http://www.haskell.org/). It will give you all you need with very little configuration but also enables you to become arbitrarily sophisticated by writing your own custom pieces of functionality. Just the right mixture of power and ease! So for now I'm really happy with this solution. I have set up an easy deployment strategy, I'm in full charge of my [nginx] web server, all javascript libraries work without an issue and I know I can always change things around if the need arises.  

## Practical Blogging with Hakyll

The actual usage of this infrastructure is a real joy: Hakyll comes with a small built in webserver that not only let's you preview your result quickly but also dynamically regenerates the static content when you change any of the sources!  
`create post in vim` -- preview in browser -- `edit post in vim` -- refresh browser -- `edit css in vim` -- refresh browser -- &#8230;  
*Much* better then any preview mechanism where that requires you to edit your sources in a web form! And if you run into any difficulties, the [guy behind it](http://jaspervdj.be/) is very helpful and there is a [google group](https://groups.google.com/forum/#!forum/hakyll) where questions get answered.  
Since deployment is now a task that has to be addressed individually, it was easy to set up a git-driven process that will publish the generated static content to my webserver. One quick `git push coldflake` now takes care that everything is safely sent to my [linode] server where a post-receive hook takes care of publishing the updated material. Having everything running like this makes blogging a joy again: I use the tools I like and don't have to fight the system anymore!

The source-code for this blog can be found [here](https://github.com/marcmo/blog.coldflake).

[Posterous]:https://posterous.com/
[Wordpress]:http://wordpress.org/
[Blogger]:http://blogger.com/
[Tumblr]:http://tumblr.com/
[Rails]:http://rubyonrails.org/
[nodejs]:http://nodejs.org/
[Express]:http://expressjs.com/
[jekyll]:https://github.com/mojombo/jekyll/wiki
[hakyll]:http://jaspervdj.be/hakyll/index.html
[markdown]:http://daringfireball.net/projects/markdown/
[pygments]:http://pygments.org/
[pandoc]:http://johnmacfarlane.net/pandoc/
[nginx]:http://nginx.org/
[linode]:http://www.linode.com/


