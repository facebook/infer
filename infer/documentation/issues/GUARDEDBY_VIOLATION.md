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

This check is enabled with `--racerd-guardedby`. In C++, it applies to fields annotated with the
clang thread safety attribute `guarded_by`, eg `int f __attribute__((guarded_by(mu)));`, in classes
that use locks.

Action: Protect the offending access by acquiring the lock indicated by the `@GuardedBy(...)`.
