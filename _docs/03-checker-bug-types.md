---
id: checkers-bug-types
title: Checkers bug types
layout: docs
permalink: /docs/checkers-bug-types.html
section: Bug Types Reference
section_order: 03
order: 02
---

## <a name="CHECKERS_IMMUTABLE_CAST"></a>Checker immutable cast

This error type is reported in Java. It fires when an immutable collection is returned from a method whose type is mutable.

```java
  public List<String> getSomeList() {
    ImmutableList<String> l = foo(...);
    return l;
  }
```

This can lead to a runtime error if users of ` getSomeList` try to modify the list e.g. by adding elements.

Action: you can change the return type to be immutable, or make a copy of the collection so that it can be modified.
