---
title: QuickChecking C++ Code
description: the haskell testing framework QuickCheck can be used quite beneficial to test C++ code
tags: haskell, C++, quickcheck, testing
---

I do a lot of embedded programming in C++ in an environment where errors are hardly tolerated. Having a solid suite of unit tests in place can go a long way in ensuring the expected program behavior. And since I like testing this is actually fun to do...  
For the code I program in haskell I usually have a different approach: I'm using [Quickcheck](http://en.wikipedia.org/wiki/QuickCheck), a tool that allows formulating properties of a program and then verifies that those properties hold by generating valid but random input data and automatically checking the properties for this input.  
Quickcheck is one of ___the___ reasons why I love programming in haskell. Once you get the hang of it you will not want to miss it anymore.

For a recent project I had to implement a service that could be used by a number of different clients whose operation depends on the current voltage level of the device they are running on. Those clients need to react properly if the voltage drops below a certain level or exceeds an upper threshold.  
Seems to be pretty straight forward. But as always there are details that complicate matters:

* _Registration:_ clients are allowed to register and unregister during all times
* _Threshold:_ each client can specify her own levels for either a lower threshold, an upper threshold, or both
* _Timeouts:_ clients can configure a timeout to specify how long an such an exceptional situation would need to prevail in order to get a notification
* _Hysteresis:_ when recovering from over/under-voltage clients can specify an [hysteresis](http://en.wikipedia.org/wiki/Hysteresis#Control_systems) to prevent a situation where it would be notified too often when the voltage level drifts around the trigger level
* ...

When implementing something like this service I tend to do a lot of unit-testing to verify correct behavior. But when the parameters of the system under test become too many it will soon become impractical to cover all possible situations with an unit-test.  
This is actually some indication that now will trigger my *quickcheck*-infiltrated brain to begin formulating properties of the system, e.g.

* circumstances under which clients will definitely have received notification
* situations in which a client that is registered will not be notified when the voltage crosses the threshold
* invariants considering the (de-)registration
* ...

Quickcheck is a valuable companion when it comes to generating a sequence of inputs that will surface weak spots in the implementation. In case of such a control-system, the inputs are actually a series of events like 

* a change in the voltage level
* a registration or de-registration of a client
* a timeout event that will trigger client notification

A stream of events will describe the behavior of the system over time. And this is what we can use to bring in quickcheck.  
Defining some data-types and a way to generate valid random data for those types will be necessry to enable quickcheck to work it's magic and find some nasty test series that will stress the system.  
Let's say we want to check the system behavior when the current is slowely rising. So we need to generate some random event stream that simulates an increasing voltage while different clients with random timeouts randomly register and unregister. So first thing that is needed is a way to generate those clients. That can be done by defining an `Arbitrary` instance for clients:

~~~ {.haskell}
data VoltageListener = VL (Int,Int,Int) deriving (Eq,Ord,Show)
instance Arbitrary VoltageListener where
  arbitrary = do
    threshold <- choose (5000,10000) 
    debounce <- elements [0,100] 
    hyst <- elements [0,100..500] 
    return $ VL (threshold,debounce,hyst)
~~~

With that in place we now can define `Arbitrary` instances for different streams, e.g. for a rising voltage:

~~~ {.haskell}
data SlowRisingCurve = SlowRising [VoltageListener] [VoltageEvent]
instance Arbitrary SlowRisingCurve where
  arbitrary = do
    vs <- eventList 350 (2,1) [100..150]
    let vss = incV 5000 vs ++ [VoltageChange 12000]
    listener <- arbitrary
    return $ SlowRising listener vss


eventList :: Int -> (Int,Int) -> [Int] -> Gen [VoltageEvent]
eventList n (freqA,freqB) range = do
    let deltaV = elements range
    let deltaVN = negate <$> elements range
    let nextEvent = frequency [(freqA,VoltageChange <$> deltaV),
                               (freqB,VoltageChange <$> deltaVN),
                               (freqA+freqB,return TimeoutEvent)]
    vectorOf n nextEvent

incV :: Int -> [VoltageEvent] -> [VoltageEvent]
incV start xs = reverse $ snd $ foldl' addEvent (start,[]) xs
  where addEvent (s,rs) (VoltageChange x) = (s+x,VoltageChange (s+x):rs)
        addEvent (s,rs) timeout = (s,timeout:rs)
~~~

Let's see what kind of events will be generated by quickcheck:

![](/images/quickcheck/raising.png)

Cool. Now let's have some fun with this and generate some very erratic curves:

![](http://www.coldflake.com/blog/images/quickcheck/scrambled.png)

Not too bad. But what is a more realistic scenario? Maybe one where the voltage level stays more or less the same but has some occasional drops due to some loads that are put in the circuit.

![](http://www.coldflake.com/blog/images/quickcheck/short_drops.png)


