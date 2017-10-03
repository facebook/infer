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

## <a name="INTERFACE_NOT_THREADSAFE"></a>Interface not thread-safe

This error indicates that you have invoked an interface method not annotated with `@ThreadSafe` from a thread-safe context (e.g., code that uses locks or is marked `@ThreadSafe`). The fix is to add the `@ThreadSafe` annotation to the interface or to the interface method. For background on why these annotations are needed, see the thread-safety [docs](http://fbinfer.com/docs/threadsafety.html#faq).


