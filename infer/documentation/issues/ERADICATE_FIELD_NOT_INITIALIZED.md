The constructor does not initialize a field f which is not annotated with
@Nullable

Example:

```java
class C {
  String f;

  C () { // field f not initialized and not annotated @Nullable
  }
}
```

Action: The preferred action is to initialize the field with a value that is not
null. If, by design, null is a valid value for the field, then it should be
annotated with @Nullable.
