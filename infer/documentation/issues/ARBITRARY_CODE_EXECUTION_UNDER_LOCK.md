A call that may execute arbitrary code (such as registered, or chained, callbacks) is made while holding a lock.
This code may deadlock whenever the callbacks obtain locks themselves, so it is an unsafe pattern.

Example:
```java
  SettableFuture future = null;

  public void callFutureSet() {
    future.set(null);
  }

  // synchronized means it's taking a lock implicitly
  public synchronized void example_of_bad_pattern() {
    callFutureSet(); // <- issue reported here
  }

  // If the call is made while holding multiple locks, the warning
  // will be issued only at the innermost lock acquisition. Here we
  // report in example_of_bad_pattern but we won't report below.
  public void nested_bad_pattern_no_report(Object o) {
    synchronized (o) {
      example_of_bad_pattern(); // <- no issue reported
    }
  }
```
