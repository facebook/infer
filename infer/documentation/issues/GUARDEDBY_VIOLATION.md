A field annotated with `@GuardedBy` is being accessed by a call-chain that starts at a non-private method without synchronization.

Example:

```java
class C {
  @GuardedBy("this")
  String f;

  void foo(String s) {
    f = s; // unprotected access here
  }
}
```

Action: Protect the offending access by acquiring the lock indicated by the `@GuardedBy(...)`.
