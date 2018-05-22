---
docid: checkers-bug-types
title: Checkers bug types
layout: docs
permalink: /docs/checkers-bug-types.html
---

## <a name="CHECKERS_IMMUTABLE_CAST"></a>Checkers immutable cast

This error type is reported in Java. It fires when an immutable collection is returned from a method whose type is mutable.

```java
  public List<String> getSomeList() {
    ImmutableList<String> l = foo(...);
    return l;
  }
```

This can lead to a runtime error if users of ` getSomeList` try to modify the list e.g. by adding elements.

Action: you can change the return type to be immutable, or make a copy of the collection so that it can be modified.

## <a name="FIELD_SHOULD_BE_NULLABLE"></a>Field should be nullable

This error type is reported in Java. It fires when a field is not marked `@Nullable`, but it is 
- Nullified in a method

```java
  private List<String> idList;
  public void reset() {
    idList = null;
    ...
  }
```

- Or tested for `null` in a method

```java
  private List<String> idList;
  public void doSomethingWithIdList() {
    if (idList == null) { ... }
  }
```

Action: 
- You may want to add `@Nullable` annotation in the field declaration. This will inform Infer that the field is intended to be set to `null` at some point. For such fields, Infer will emit a warning if you forget to check for `null` before accessing them.

```java
  import javax.annotation.Nullable;
  ...
  private @Nullable List<String> idList;
  public void doSomethingWithIdList() {
    int numIds = idList.size();  // Infer will complain that idList is not null-checked here
    ...
  }
```
- If the field is never intended to be nullable, please refactor your codes so that it will never be assigned or compared with `null`.
```java
  private List<String> idList = new List<String>();
  ...
```

## <a name="FRAGMENT_RETAINS_VIEW"></a>Fragment retains view

This error type is Android-specific. It fires when a `Fragment` type fails to nullify one or more of its declared `View` fields in `onDestroyView`. In performance-sensitive applications, a `Fragment` should initialize all `View`'s in `onCreateView` and nullify them in `onDestroyView`. If a `Fragment` is placed on the back stack and fails to nullify a `View` in `onDestroyView`, it will retain a useless reference to that `View` that will not be cleaned up until the `Fragment` is resumed or destroyed.

Action: Nullify the `View` in question in `onDestroyView`.

## <a name="STATIC_INITIALIZATION_ORDER_FIASCO"></a>Static initialization order fiasco

This error is reported in C++. It fires when the initialization of a static variable `A`, accesses a static variable `B` from another translation unit (usually another `.cpp` file). There are no guarantees whether `B` has been already initialized or not at that point.

For more technical definition and techniques to avoid/remediate, see the [FAQ](https://isocpp.org/wiki/faq/ctors#static-init-order).

## <a name="DEAD_STORE"></a>Dead store

This error is reported in C++. It fires when the value assigned to a variables is never used (e.g., `int i = 1; i = 2; return i;`).

## <a name="THREAD_SAFETY_VIOLATION"></a>Thread-safety violation

This error indicates a possible race condition--see the thread-safety [docs](http://fbinfer.com/docs/threadsafety.html) for more details.

## <a name="INTERFACE_NOT_THREAD_SAFE"></a>Interface not thread-safe

This error indicates that you have invoked an interface method not annotated with `@ThreadSafe` from a thread-safe context (e.g., code that uses locks or is marked `@ThreadSafe`). The fix is to add the `@ThreadSafe` annotation to the interface or to the interface method. For background on why these annotations are needed, see the detailed explanation [here](http://fbinfer.com/docs/threadsafety.html#interface-not-thread-safe).

## <a name="DEADLOCK"></a>Deadlock

This error is currently reported in Java.  A deadlock occurs when two distinct threads try to acquire two locks in reverse orders.  The following code illustrates a textbook example.  Of course, in real deadlocks, the lock acquisitions may be separated by deeply nested call chains.  

```java
  public void lockAThenB() {
    synchronized(lockA) {
      synchronized(lockB) {
       // do something with both resources
      }
    }
  }

  public void lockBThenA() {
    synchronized(lockB) {
      synchronized(lockA) {
       // do something with both resources
      }
    }
  }
```

The standard solution to a deadlock is to fix an order of lock acquisition and adhere to that order in all cases.  Another solution may be to shrink the critical sections (i.e., the code executing under lock) to the minimum required.

Old-style containers such as `Vector` are synchronized on the object monitor, which means that deadlocks can occur even without explicit synchronisation on both threads.  For instance:

```java
  public void lockAThenAddToVector() {
    synchronized(lockA) {
      vector.add(object);
    }
  }

  public void lockVectorThenA() {
    synchronized(vector) {
      synchronized(lockA) {
       // do something with both resources
      }
    }
  }
```

Infer has support for detecting these deadlocks too.

## <a name="STARVATION"></a>UI Thread Starvation

This error is reported in Java, and specifically on Android.  These reports are triggered when a method that runs on the UI thread may block, thus potentially leading to an Application Not Responding error.

Infer considers a method as running on the UI thread whenever:
- The method, one of its overrides, its class, or an ancestral class, is annotated with `@UiThread`.
- The method, or one of its overrides is annotated with `@OnEvent`, `@OnClick`, etc.
- The method or its callees call a `Litho.ThreadUtils` method such as `assertMainThread`.

The issue is reported when a method deemed to run on the UI thread
- Makes a method call which may block.
- Takes a lock, and another thread takes the same lock, and before releasing it, makes a call that may block.

Calls that may block are considered:
- Certain I/O calls.
- Two way `Binder.transact` calls.
- Certain OS calls.
- `Future` or `AsyncTask` calls to `get` without timeouts, or with too large timeouts.
