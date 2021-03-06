---
title: Testing Without A Brain
description: How much Testing is good for your Development Process?
tags: testing, rant
---

Testing software is perhaps one of the most controversial topics among programmers. Opinions range
from "test first and test everything" to "tests slow us down". As with so many things, the truth
lies somewhere in between. After making my own experiences in a lot of different projects, I have to
admit to myself that finding the perfect balance is still a hard a lot of times.

Starting out in a new project, time pressure usually is low and we programmers are highly
motivated to get our hands dirty with coding rather than eliciting yet another requirement. I found
that people tend to write unit tests more readily at this stage. And arguably the code quality seems
to be better than when working under tight deadlines. But that's not necessarily related to the
tests, it could very well be attributed to the relative freshness of the people on the project.  
As projects get into a more intense phase the ratio between test-code and production-code usually
takes a hit. So is this a bad thing?

## How much is too much

Kent Beck, the originator of the term *Test Driven Development* for sure has a ton of experience
in writing software and in testing it. It might come as a surprise for many that he draws a pretty
tight line between what is actually too much testing[^1]:

> I get paid for code that works, not for tests, so my philosophy is to test as little as possible
> to reach a given level of confidence...
>
> If I don't typically make a kind of mistake ... I don't
> test for it.

To me that sounds very reasonable. Blindly striving for a high test coverage just doesn't cut it. On
the contrary: if a programmer is just guided by his coverage report, he will probably write some
hard to maintain test code. And hard-to-maintain test code will eventually lead to reduced testing
since the damn tests require more and more work to keep them in sync with changes made to the
software. Those tests will get kicked out -- and that with good reason!

There is another issue I found with some of my unit testing: Overuse of abstractions. While I'd
consider abstractions in general a desirable and good thing, they really only make sense when there
is the actual need for an abstraction. If you only have one implementation of some functionality in
your code, why go through the effort of generalizing? Since by definition there is only one thing to
abstract about, your abstraction will probably have to be refined when introducing a second
implementation.  
But writing unit-tests mandates the use of abstractions since you only want to test some piece of
code in isolation of the rest of the system -- for which some kind of interface definitions come in
handy. So you bite the bullet and create your interfaces, change the code around, build mocks and
other scaffolding and hours later start writing your tests...while at the same time you could have
cranked out this extra feature or stomped this nasty bug. Sounds like a tough choice to me.

## The kind of software

When people show me their incredibly cool new test framework or preach about the absolute imperative
of having thoroughly unit-tested code, the examples they come up with usually lend themselves to a
unit-test centric approach. But we rarely have to write the next sorting algorithm or code to
serialize and de-serialize data.  
The day-to-day stuff often involves database access, updating a UI,
network code a.s.o. Those things are by far harder to cover by unit-tests and tend to involve some
heavy machinery. Time spent with setting up unit-tests might be spent more effectively otherwise.  

## The kind of testing

I still consider testing my software with due diligence an integral part of the job. For some code
this might very well be exhaustive unit testing, in other cases a different strategy might prove
the better choice -- like moving more into a more global approach where the functionality is tested
as part of a bigger use-case. This kind of testing -- often called integration-testing -- is much
harder to automate and thus does not always allow for regression testing, but it can be the right
choice and help you to implement *and* test your code more quickly.  
There is another very different flavor of testing I so far omitted: the kind of testing in a
dynamically typed language that makes sure that trivial (or not so trivial) changes do not completely
mess up your code-base. Those tests -- I'd rather call them *syntactic checks* -- are a
prerequisite to stay sane. I did a couple of larger ruby programs and it was really painful to come
back to the code at some point later: to find out I had a hard time to introduce changes just
because I broke the code in unintended ways and my test coverage was not high enough to detect it.

## Striking a balance

So what would be good advice for that new guy that started in your C++ department yesterday. Test
everything? Strive for high coverage? I'd say yes -- but as with every advice this should be taken
with a grain of salt. If you end up with double the amount of code and possibly a different design
I'd say you clearly overshot. In the end it's not the unit-tests that are responsible for great
software -- it's the talent and the ability of the developer.  
On the other hand if the new guy writes your next rails application, the answer is much simpler:
test the shit out of the code!

## The Feedback Loop

[Edit] After some discussion I had at my office I realized I forgot to mention a very important detail that
might well favor the *test first and test everything* approach -- the feedback loop. No matter if you
use unit-tests, integration tests or any other kind of process that helps you validate your software
behaves as expected: you absolutely got to have some way of harvesting feedback.

~~~ {.ruby}
    while (!ready)
      change code
      compile # if you program in a compiled language
      deploy
      run
~~~

Developing a smartphone app might incur enough of a delay in this cycle to make you fly blind for a
prolonged period of time. I find that less of a problem since this loop can usually be optimized
pretty well (even though I mostly develop for embedded targets). But it's not unheard of that in
some organizations (e.g. an unnamed company I was working for in the past) not every developer has
the luxury of his own setup and sometimes even has to share the license for the compiler. If you are
stuck in such a situation a comprehensive suite of unittests will definitely make your life easier!

-----

[Edit 2] I just found a nice little [article on software testing] that contains some very true points
and one really nice quote [from Edsger W. Dijkstra]:

> Today a usual technique is to make a program and then to test it. But: program testing can be a very
> effective way to show the presence of bugs, but is hopelessly inadequate for showing their absence.

[^1]: see Kent Beck's reply on stackoverflow to ["How deep are unit tests"]

["How deep are unit tests"]:http://stackoverflow.com/questions/153234/how-deep-are-your-unit-tests
[from Edsger W. Dijkstra]:https://www.cs.utexas.edu/~EWD/transcriptions/EWD03xx/EWD340.html
[article on software testing]:http://stoneship.org/essays/tdd-bypasses-your-brain/

