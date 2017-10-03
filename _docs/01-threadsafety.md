---
docid: threadsafety
title: "Infer : Thread-Safety Checker"
layout: docs
permalink: /docs/threadsafety.html
---

Infer's thread-safety analysis find data races in your Java code. 
To run the analysis, you can use `infer -a checkers` (to run the thread-safety analysis along with other AI's) or `infer -a checkers --no-default-checkers --threadsafety` (to run only the thread-safety analysis).
The annotations described below can be used via the Maven Central package [here](https://maven-repository.com/artifact/com.facebook.infer.annotation/infer-annotation/0.10.0.2).

## Background

Infer statically analyzes Java code to detect potential concurrency bugs. This analysis does not attempt to prove thread safety, rather, it searches for some typical thread safety problems. At the moment Infer concentrates on race conditions between methods in a class that is itself intended to be thread safe. A race condition occurs when there are two concurrent accesses to a class member variable that are not separated by mutual exclusion, and at least one of the accesses is a write. Mutual exclusion can be obtained by synchronization primitives such as locks, or by knowledge that both accesses occur on the same thread.

## Unprotected write warnings

Given a class annotated with `@ThreadSafe`, Infer will report whether that class causes any writes which are not guarded by a lock or `synchronized` block.

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

The class `Dinner` will generate the following report on the public method `makeDinner()`:

`There may be a Thread Safety Violation: makeDinner() indirectly writes to mTemperature outside of synchronization.`

Fix the warning by synchronizing the access to `mTemperature`, making `mTemperature` `volatile`, or suppress the warning by changing the class annotation to `@ThreadSafe(enableChecks = false)`. See the `com.facebook.infer.annotation` package for the full details on this and other annotations.

## Read/Write Race Warning

We sometimes need to protect read accesses as well as writes. Consider the following class with unsynchronized methods.

```
@ThreadSafe
public class Account {

  int balance = 0;

  public void deposit(int amount) {
    if (amount > 0) {
      balance = balance + amount;
    } 
  }

  public int withdraw(int amount){
    if (amount >= 0 && balance - amount >= 0) {
      balance = balance - amount;
      return balance;
    } else {
      return 0;
    }
  }
}
```

If you run the `withdraw()` method in parallel with itself or with `deposit()` you can get unexpected results here. For instance, if the stored balance is 11 and you run `withdraw(10)` in parallel with itself you can get a negative balance. Furthermore, if you synchronize only the write statement `balance = balance - amount`, then you can still get this bad result. The reason is that there is a read/write race between the boolean condition `balance - amount >= 0` and the writes. Infer will duly warn

`Read/Write race. Public method int Account.withdraw(int) reads from field Account.balance. Potentially races with writes in methods void Account.deposit(int), int Account.withdraw(int)`

on the line with this boolean condition.

A solution to the threading problem here is to make both methods `synchronized` to wrap both read and write accesses, and not only the writes.

## Annotations: `@ThreadConfined`

The intuitive idea of thread-safety is that a class is impervious to concurrency issues for all concurrent contexts, even those that have not been written yet (it is future proof). Infer implements this by naively assuming that any method can potentially be called on any thread. You may determine, however, that an object, method, or field is only ever accessed on a single thread during program execution. Annotating such elements with `@ThreadConfined` informs Infer of this restriction. Note that a thread-confined method cannot race with itself but it can still race with other methods.

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
In this example, both `prepareCache` and `run` touch `mCache`. But there's no possibility of a race between the two methods because both of them will run sequentially on the UI thread. Adding a `@ThreadConfined(UI)` annotation to these methods will stop it from warning that there is a race on `mCache`. We could also choose to add a `@ThreadConfined` annotation to `mCache` itself.

## Annotations: `@Functional`
Some writes may be race safe - ie. regardless of program execution order, the write will always be the same value. Consider the method `helloWorld()` which caches the string `"Hello World"`. The write to `mHelloWorld` is unprotected and Infer would normally generate a warning. Yet, the write is safe because (a) `concat(String a, String b)` always returns an equivalent value for equivalent parameters, and (b) this is the only write to `mHelloWorld` (THIS IS IMPORTANT! And make sure it's true, because Infer won't check that for you). `concat()` can be annotated with `@Functional` to suppress the warning.

```
private String mHelloWorld;

@ThreadSafe
private String helloWorld() {
  if (mHelloWorld == null) {
    // This write is unprotected but safe.
    mHelloWorld = concat("Hello ", "World");
  }
  return mHelloWorld;
}

@Functional
private String concat(String a, String b) {
  return a + b
}
```

Be sure not to use the `@Functional` pattern for *singleton instantiation*, as it's possible the “singleton” can be constructed more than once.

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

## Annotations: `@ReturnsOwnership`
Infer does not warn on unprotected writes to “owned” objects. A method “owns” an object if that object is only referenced by local variables. Infer automatically tracks ownership through non-overridden methods but needs help with polymorphic ones. Annotate super-class and interface methods with `@ReturnsOwnership` if they return an object without saving it.

```
@ThreadSafe
public interface Car {
  @ReturnsOwnership abstract Car buyCar();
  
  void carsStuff() {
    Car myCar = new Car();
    myCar.wheels = 4; // Infer won't warn here because it knows myCar is owned
    Car otherCar = buyCar();
    otherCar.wheels = 3; // Infer would normally warn here, but won't because of the `@ReturnsOwnership` annotation
  }
}
```

## Limitations
There are many types of concurrency issues out there that Infer does not check for (but might in the future). Examples include deadlock, thread confined objects escaping to other threads, and check-then-act bugs (shown below). You must look for these bugs yourself!

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
Unfortunately, using `synchronized` blindly as a means to fix every unprotected write or read is not always safe. Even with Infer, finding, understanding, and fixing concurrency issues is difficult. If you would like to learn more about best practices, [Java Concurrency in Practice](http://jcip.net/) is an excellent resource.

## FAQ

### Why do I need to annotate interfaces?

Good question. This is a fine idea in principle, but a bad idea in practice due to a (a) separate compilation and (b) our diff-based deployment model. Consider the following example:

```
@ThreadSafe void foo(Interface i) {
  i.bar();
}
```

The compiler doesn't have to know about all implementations (or indeed, any implementations) of `Interface` at the time it compiles this code, so there's no guarantee that Infer will know about or be able to check all implementations of `Interface`. That's (a).
For (b), say that we check that all implementations of `Interface` are thread-safe at the time this code is written, but we don't add the annotation. If someone else comes along and adds a new implementation of `Interface` that is not thread-safe, Infer will have no way of knowing that this will cause a potential bug in `foo`. But if `Interface` is annotated, Infer will enforce that all new implementations of `Interface` are thread-safe, and `foo` will remain bug-free.
