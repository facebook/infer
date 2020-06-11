Method m can return null, but the method's return type is not annotated with
@Nullable

Example:

```java
class C {
  String m() {
    return null;
  }
}
```

Action: The preferred action is to ensure that a null value is never returned by
the method, by changing the code or changing annotations. If this cannot be
done, add a @Nullable annotation to the the method declaration. This annotation
might trigger more warnings in the callers of method m, as the callers must now
deal with null values.
