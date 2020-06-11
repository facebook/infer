A parameter of the overridden method is missing a @Nullable annotation present in the superclass.

Action: choose a consistent annotation based on the desired invariant.

Example:

```java
class A {

  int len(@Nullable String s) {
    if (s != null) {
      return s.length();
    } else {
      return 0;
    }
  }
}

class B extends A {

  int len(String s) {  // @Nullable missing.
    return s.length();
  }
}
```

A consistent use of @Nullable on parameters across subtyping should prevent runtime issue like in:

```java
public class Main {

  String s;

  int foo() {
    A a = new B();
    return a.len(s);
  }
}
```

