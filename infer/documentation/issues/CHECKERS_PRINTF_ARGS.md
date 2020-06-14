This error is reported when the argument types to a `printf` method do not match the format string.

```java
  void stringInsteadOfInteger(PrintStream out) {
    out.printf("Hello %d", "world");
  }
```

Action: fix the mismatch between format string and argument types.
