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

## <a name="FRAGMENT_RETAINS_VIEW"></a>Fragment retains view

This error type is Android-specific. It fires when a `Fragment` type fails to nullify one or more of its declared `View` fields in `onDestroyView`. In performance-sensitive applications, a `Fragment` should initialize all `View`'s in `onCreateView` and nullify them in `onDestroyView`. If a `Fragment` is placed on the back stack and fails to nullify a `View` in `onDestroyView`, it will retain a useless reference to that `View` that will not be cleaned up until the `Fragment` is resumed or destroyed.

Action: Nullify the `View` in question in `onDestroyView`.
