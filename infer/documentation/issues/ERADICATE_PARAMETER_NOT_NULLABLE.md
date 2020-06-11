Method call x.m(..., v, ...) where v can be null and the corresponding parameter
in method m is not annotated with @Nullable

Example:

```java
class C {
  void m(C x) {
    String s = x.toString()
  }

  void test(@Nullable C x) {
    m(x);
  }
}
```

Action: The preferred action is to ensure that a null value is never passed to
the method, by changing the code or changing annotations. If this cannot be
done, add a @Nullable annotation to the relevant parameter in the method
declaration. This annotation might trigger more warnings in the implementation
of method m, as that code must now deal with null values.
