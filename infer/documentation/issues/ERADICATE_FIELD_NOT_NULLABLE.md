An assignment x.f = v where v could be null and field f is not annotated with
@Nullable.

Example:

```java
class C {
  String f;

  void foo(@Nullable String s) {
    f = s;
  }
}
```

Action: The preferred action is to ensure that a null value is never stored in
the field, by changing the code or changing annotations. If this cannot be done,
add a @Nullable annotation to the field. This annotation might trigger more
warnings in other code that uses the field, as that code must now deal with null
values.
