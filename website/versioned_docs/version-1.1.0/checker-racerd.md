---
title: "RacerD"
description: "Thread safety analysis."
---

Thread safety analysis.

Activate with `--racerd`.

Supported languages:
- C/C++/ObjC: Yes
- Java: Yes
- C#/.Net: Yes

RacerD finds data races in your C++/Objective C and Java code. This page gives a more in-depth
explanation of how the analysis works *for Java code*, but may be less complete than the
[Thread Safety Violation bug description page](/docs/1.1.0/all-issue-types#thread_safety_violation).
For information on C++ and Objective C, see the
[Lock Consistency violation page](/docs/1.1.0/all-issue-types#lock_consistency_violation).

To run the analysis, you can use plain `infer` (to run RacerD along with other
analyses that are run by default) or `infer --racerd-only` (to run only RacerD).

For example, the command `infer --racerd-only -- javac File.java` will run
RacerD on File.java.

## Background

RacerD statically analyzes Java code to detect potential concurrency bugs. This
analysis does not attempt to prove the absence of concurrency issues, rather, it
searches for a high-confidence class of data races. At the moment RacerD
concentrates on race conditions between methods in a class that is itself
intended to be thread safe. A race condition occurs when there are two
concurrent accesses to a class member variable that are not separated by mutual
exclusion, and at least one of the accesses is a write. Mutual exclusion can be
ensured by synchronization primitives such as locks, or by knowledge that both
accesses occur on the same thread.

## Triggering the analysis

RacerD doesn't try to check _all_ code for concurrency issues; it only looks at
code that it believes can run in a concurrent context. There are two signals
that RacerD looks for: (1) Explicitly annotating a class/method with
`@ThreadSafe` and (2) using a lock via the `synchronized` keyword. In both
cases, RacerD will look for concurrency issues in the code containing the signal
and all of its dependencies. In particular, it will report races between any
non-`private` methods of the same class that can perform conflicting accesses.
Annotating a class/interface with `@ThreadSafe` also triggers checking for all
of the subclasses of the class/implementations of the interface.

## Warnings

Let's take a look at the different types of concurrency issues that RacerD
flags. Two of the warning types are data races (`Unprotected write` and
`Read/write race`), and the third warning type encourages adding `@ThreadSafe`
annotations to interfaces to trigger additional checking.

### Unprotected write

RacerD will report an unprotected write when one or more writes can run in
parallel without synchronization. These come in two flavors: (1) a self-race (a
write-write race that occurs due to a method running in parallel with itself)
and (2) two conflicting writes to the same location. Here's an example of the
self-race flavor:

```
@ThreadSafe
public class Dinner {
  private int mTemperature;

  public void makeDinner() {
    boilWater();
  }

  private void boilWater() {
    mTemperature = 100; // unprotected write.
  }
}
```

The class `Dinner` will generate the following report on the public method
`makeDinner()`:

`There may be a Thread Safety Violation: makeDinner() indirectly writes to mTemperature outside of synchronization.`

This warning can be fixed by synchronizing the access to `mTemperature`, making
`mTemperature` `volatile`, marking `makeDinner` as `@VisibleForTesting`, or
suppressing the warning by annotating the `Dinner` class or `makeDinner` method
with `@ThreadSafe(enableChecks = false)`.

### Read/Write Race

We sometimes need to protect read accesses as well as writes. Consider the
following class with unsynchronized methods.

```
@ThreadSafe
public class Account {

  int mBalance = 0;

  public void deposit(int amount) {
    if (amount > 0) {
      mBalance += amount;
    }
  }

  public int withdraw(int amount){
    if (amount >= 0 && mBalance - amount >= 0) {
      mBalance -= amount;
      return mBalance;
    } else {
      return 0;
    }
  }
}
```

If you run the `withdraw()` method in parallel with itself or with `deposit()`
you can get unexpected results here. For instance, if the stored balance is 11
and you run `withdraw(10)` in parallel with itself you can get a negative
balance. Furthermore, if you synchronize only the write statement
`mBalance -= amount`, then you can still get this bad result. The reason is that
there is a read/write race between the boolean condition
`mBalance - amount >= 0` and the writes. RacerD will duly warn

`Read/Write race. Public method int Account.withdraw(int) reads from field Account.mBalance. Potentially races with writes in methods void Account.deposit(int), int Account.withdraw(int)`

on the line with this boolean condition.

A solution to the threading problem here is to make both methods `synchronized`
to wrap both read and write accesses, or to use an `AtomicInteger` for
`mBalance` rather than an ordinary `int`.

### Interface not thread-safe

In the following code, RacerD will report an `Interface not thread-safe` warning
on the call to `i.bar()`:

```
interface I {
  void bar();
}

@ThreadSafe
class C {
  void foo(I i) {
    i.bar(); // RacerD warns here
  }
}
```

The way to fix this warning is to add a `@ThreadSafe` annotation to the
interface `I`, which will enforce the thread-safety of each of the
implementations of `I`.

You might wonder why it's necessary to annotate `I` -- can't RacerD just look at
all the implementations of `i` at the call site for `bar`? Although this is a
fine idea in principle, it's a bad idea in practice due to a (a) separate
compilation and (b) our diff-based deployment model. In the example above, the
compiler doesn't have to know about all implementations (or indeed, any
implementations) of `I` at the time it compiles this code, so there's no
guarantee that RacerD will know about or be able to check all implementations of
`I`. That's (a). For (b), say that we check that all implementations of `I` are
thread-safe at the time this code is written, but we don't add the annotation.
If someone else comes along and adds a new implementation of `I` that is not
thread-safe, RacerD will have no way of knowing that this will cause a potential
bug in `foo`. But if `I` is annotated, RacerD will enforce that all new
implementations of `I` are thread-safe, and `foo` will remain bug-free.

## Annotations to help RacerD understand your code

Getting started with RacerD doesn't require any annotations at all -- RacerD
will look at your usage of locks and figure out what data is not guarded
consistently. But increasing the coverage and signal-to-noise ratio may require
adding `@ThreadSafe` annotations along with some of the other annotations
described below. Most of annotations described below can be used via the Maven
Central package available
[here](https://maven-repository.com/artifact/com.facebook.infer.annotation/infer-annotation).

### `@ThreadConfined`

The intuitive idea of thread-safety is that a class is impervious to concurrency
issues for all concurrent contexts, even those that have not been written yet
(it is future-proof). RacerD implements this by naively assuming that any method
can potentially be called on any thread. You may determine, however, that an
object, method, or field is only ever accessed on a single thread during program
execution. Annotating such elements with `@ThreadConfined` informs RacerD of
this restriction. Note that a thread-confined method cannot race with itself but
it can still race with other methods.

```
List mCache;

@ThreadConfined(UI)
void prepareCache() {
  // populate the cache
  mCache.add(...);
  // post cache cleanup task to run later
  mUIExecutor.execute(new Runnable() {
    @ThreadConfined(UI)
    public void run() {
      mCache.clear();
    }
  });
}
```

In this example, both `prepareCache` and `run` touch `mCache`. But there's no
possibility of a race between the two methods because both of them will run
sequentially on the UI thread. Adding a `@ThreadConfined(UI)` or `@UiThread`
annotation to these methods will stop it from warning that there is a race on
`mCache`. We could also choose to add a `@ThreadConfined` annotation to `mCache`
itself.

### `@Functional`

Not all races are bugs; a race can be benign. Consider the following:

```
@Functional Boolean askNetworkIfShouldShowFeature();

private Boolean mShouldShowFeature;

@ThreadSafe boolean shouldShowFeature() {
  if (mShouldShowFeature == null) {
    mShouldShowFeature = askNetworkIfShouldShowFeature();
  }
  return mShouldShowFeature;
}
```

This code caches the result of an expensive network call that checks whether the
current user should be shown an experimental feature. This code looks racy, and
indeed it is: if two threads execute `shouldShowFeature()` at the same time, one
may read `mShouldShowFeature` at the same time the other is writing it.

However, this is actually a _benign_ race that the programmer intentionally
allows for performance reasons. The reason this code is safe is that the
programmer knows that `askNetworkIfShouldShowFeature()` will always return the
same value in the same run of the app. Adding synchronization would remove the
race, but acquiring/releasing locks and lock contention would potentially slow
down every call to `shouldShowFeature()`. The benign race approach makes every
call after the first fast without changing the safety of the code.

RacerD will report a race on this code by default, but adding the
`@Functional annotation to askNetworkIfShouldShowFeature()` informs RacerD that
the function is always expected to return the same value. This assumption allows
RacerD to understand that this particular code is safe, though it will still
(correctly) warn if `mShouldShowFeature` is read/written elsewhere.

Be sure not to use the `@Functional` pattern for _singleton instantiation_, as
it's possible the "singleton" can be constructed more than once.

```
public class MySingleton {
  private static sInstance;

  // Not @Functional
  public MySingleton getInstance() {
    if (sInstance == null) {
      // Different threads may construct their own instances.
      sInstance == new MySingleton();
    }
    return sInstance;
  }
}
```

### `@ReturnsOwnership`

RacerD does not warn on unprotected writes to _owned_ objects. An object is
owned if it has been freshly allocated in the current thread and has not escaped
to another thread. RacerDf automatically tracks ownership in most cases, but it
needs help with `abstract` and `interface` methods that return ownership:

```
@ThreadSafe
public interface Car {
  @ReturnsOwnership abstract Car buyCar();

  void carsStuff() {
    Car myCar = new Car();
    myCar.wheels = 4; // RacerD won't warn here because it knows myCar is owned
    Car otherCar = buyCar();
    otherCar.wheels = 3; // RacerD would normally warn here, but won't because of the `@ReturnsOwnership` annotation
  }
}
```

### `@VisibleForTesting`

RacerD reports races between any two non`-private` methods of a class that may
run in a concurrent context. Sometimes, a RacerD report may be false because one
of the methods cannot actually be called from outside the current class. One fix
is making the method `private` to enforce this, but this might break unit tests
that need to call the method in order to test it. In this case, the
`@VisibleForTesting` annotation will allow RacerD to consider the method as
effectively `private` and will still allow it to be called from the unit test:

```
@VisibleForTesting void setF() {
  this.f = ...; // RacerD would normally warn here, but @VisibleForTesting will silence the warning
}

synchronized void setFWithLock() {
  setF();
}
```

Unlike the other annotations shown here, this one lives in
[Android](https://developer.android.com/reference/android/support/annotation/VisibleForTesting.html).

## Interprocedural Reasoning

An important feature of RacerD is that it finds races by analyzing not just one
file or class, but by looking at memory accesses that occur after going through
several procedure calls. It handles this even between classes and between files.

Here is a very basic example

```
@ThreadSafe
class A{

  void m1(B bb) {
    bb.meth_write();
  }
}

class B{
 Integer x;

 void meth_write() {
   x = 88;
 }

}
```

Class `B` is not annotated `@ThreadSafe` and does not have any locks, so RacerD
does not directly look for threading issues there. However, method `m1()` in
class `A` has a potential self-race, if it is run in parallel with itself and
the same argument for each call. RacerD discovers this.

```
InterProc.java:17: error: THREAD_SAFETY_VIOLATION
  Unprotected write. Non-private method `A.m1` indirectly writes to field `&this.B.x` outside of synchronization.
 Reporting because the current class is annotated `@ThreadSafe`, so we assume that this method can run in
 parallel with other non-private methods in the class (incuding itself).
  15.
  16.     void m1(B bb) {
  17. >     bb.meth_write();
  18.     }
  19.   }
```

RacerD does this sort of reasoning using what is known as a _compositional
inteprocedural analysis_. There, each method is analyzed independently of its
context to produce a summary of the behaviour of the procedure. In this case the
summaries for `m1()' and`meth()' include information as follows.

```
Procedure: void A.m1(B)
Accesses: { Unprotected({ 1 }) -> { Write to &bb.B.x at void B.meth_write() at line 17 } }

Procedure: void B.meth_write()
Accesses { Unprotected({ 0 }) -> { Write to &this.B.x at  at line 25 } }
```

The descriptions here are cryptic and do not include all the information in the
summaries, but the main point is that you can use RacerD to look for races in
codebases where the mutations done by threads might occur only after a chain of
procedure calls.

## <a name="context"></a> Context and Selected Related Work

Reasoning about concurrency divides into bug detection and proving absence of
bugs. RacerD is on the detection side of reasoning.

The rapid growth in the number of interleavings is problematic for tools that
attempt exhaustive exploration. With just 150 instructions for two threads, the
number 10^88 of interleavings is more that the estimated number of atoms in the
known universe.
[There has been important work which uses various techniques to attempt to reduce the number of interleavings](https://en.wikipedia.org/wiki/Partial_order_reduction)
while still in principle covering all possibilities, but scale is still a
challenge. Note that RacerD is not exhaustive: it has false negatives (missed
bugs). But in compensation it is fast, and effective (it finds bugs in
practice).

Static analysis for concurrency has attracted a lot of attention from
researchers, but difficulties with scalability and precision have meant that
previous techniques have had little industrial impact. Automatic static race
detection itself has seen significant work. The most advanced approaches,
exemplified by the [Chord](http://www.cis.upenn.edu/~mhnaik/pubs/pldi06.pdf)
tool, often use a whole-program analysis paired with a sophisticated alias
analysis, two features we have consciously avoided. Generally speaking, the
leading research tools can be more precise, but RacerD is faster and can operate
without the whole program: we have opted to go for speed in a way that enables
industrial deployment on a large, rapidly changing codebase, while trying to use
as simple techniques as possible to cover many (not all) of the patterns covered
by slower but precise research tools.

An industrial static analysis tool from
[Contemplate](http://homepages.inf.ed.ac.uk/dts/pub/avocs2015.pdf) also targets
@ThreadSafe annotations, but limits the amount of inter-procedural reasoning:
“This analysis is interprocedural, but to keep the overall analysis scalable,
only calls to private and protected methods on the same class are followed”.
RacerD does deep, cross-file and cross-class inter-procedural reasoning, and yet
still scales; the inter-class capability was one of the first requests from
Facebook engineers.
[A separate blog post looked at 100 recent data race fixes](https://code.facebook.com/posts/1537144479682247/finding-inter-procedural-bugs-at-scale-with-infer-static-analyzer/)
in Infer's deployment in various bug categories, and for data races observed
that 53 of them were inter-file (and thus involving multiple classes).
[See above](#interprocedural-reasoning) for an example of RacerD's interprocedural
capabilities.

One reaction to the challenge of developing effective static race detectors has
been to ask the programmer to do more work to help the analyzer. Examples of
this approach include the
[Clang Thread Safety Analyzer](https://clang.llvm.org/docs/ThreadSafetyAnalysis.html),
the typing of [locks](https://doc.rust-lang.org/std/sync/struct.Mutex.html) in
Rust, and the use/checking of @GuardedBy annotations in
[Java](https://homes.cs.washington.edu/~mernst/pubs/locking-semantics-nfm2016.pdf)
including in
[Google's Error Prone analyzer](https://github.com/google/error-prone/blob/master/docs/bugpattern/GuardedBy.md).
When lock annotations are present they make the analyzer's life easier. It is possible to have a very effective race analysis without decreeing
that such annotations must be present. This was essential for our deployment,
since _requiring_ lock annotations would have been a show stopper for converting
many thousands of lines of code to a concurrent context. We believe that this
finding should be transportable to new type systems and language designs, as
well as to other analyses for existing languages.

Another reaction to difficulties in static race detection has been to instead
develop dynamic analyses, automatic testing tools which work by running a
program to attempt to find flaws. Google's Thread Sanitizer is a widely used and
mature tool in this area, which has been used in production to find many bugs in
C-family languages.
[The Thread Sanitizer authors explicitly call out limitations with static race analyzers](http://www.cs.columbia.edu/~junfeng/11fa-e6121/papers/thread-sanitizer.pdf)
as part of their motivation: “It seems unlikely that static detectors will work
effectively in our environment: Google’s code is large and complex enough that
it would be expensive to add the annotations required by a typical static
detector”.

We have worked to limit the annotations that RacerD needs, for reasons similar
those expressed by the Thread Sanitizer authors. And we have sought to bring the
complementary benefits of static analysis — possibility of cheaper analysis and
fast reporting, and ability to analyze code before it is placed in a context to
run — to race detection. But we are interested as well in the future in
leveraging ideas in the dynamic techniques to improve or add to our analysis for
race detection.

## Limitations

There are a number of known limitations to the design of the race detector.

- It looks for races involving syntactically identical access paths, and misses
  races due to aliasing
- It misses races that arise from a locally declared object escaping its scope
- It uses a boolean locks abstraction, and so misses races where two accesses
  are mistakenly protected by different locks
- It assumes a deep ownership model, which misses races where local objects
  refer to or contain non-owned objects.
- It avoids reasoning about weak memory and Java's volatile keyword

Most of these limitations are consistent with the design goal of reducing false
positives, even if they lead to false negatives. They also allow technical
tradeoffs which are different than if we were to favour reduction of false
negatives over false positives.

A different kind of limitation concerns the bugs searched for: Data races are
the most basic form of concurrency error, but there are many types of
concurrency issues out there that RacerD does not check for (but might in the
future). Examples include deadlock, atomicity, and check-then-act bugs (shown
below). You must look for these bugs yourself!

```
@ThreadSafe
public class SynchronizedList<T> {
  synchronized boolean isEmpty() { ... }
  synchronized T add(T item) { ... }

// Not thread safe!!!
public class ListUtil<T> {
  public void addIfEmpty(SynchronizedList<T> list, T item) {
    if (list.isEmpty()) {
      // In a race, another thread can add to the list here.
      list.add(item);
    }
  }
}
```

Finally, using `synchronized` blindly as a means to fix every unprotected write
or read is not always safe. Even with RacerD, finding, understanding, and fixing
concurrency issues is difficult. If you would like to learn more about best
practices, [Java Concurrency in Practice](http://jcip.net/) is an excellent
resource.


## List of Issue Types

The following issue types are reported by this checker:
- [GUARDEDBY_VIOLATION](/docs/1.1.0/all-issue-types#guardedby_violation)
- [GUARDEDBY_VIOLATION_NULLSAFE](/docs/1.1.0/all-issue-types#guardedby_violation_nullsafe)
- [INTERFACE_NOT_THREAD_SAFE](/docs/1.1.0/all-issue-types#interface_not_thread_safe)
- [LOCK_CONSISTENCY_VIOLATION](/docs/1.1.0/all-issue-types#lock_consistency_violation)
- [THREAD_SAFETY_VIOLATION](/docs/1.1.0/all-issue-types#thread_safety_violation)
- [THREAD_SAFETY_VIOLATION_NULLSAFE](/docs/1.1.0/all-issue-types#thread_safety_violation_nullsafe)
