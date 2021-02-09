A call which may execute arbitrary code (such as registered, or chained, callbacks) is made while a lock is held.
This code may deadlock whenever the callbacks obtain locks themselves, so it is an unsafe pattern.
This warning is issued only at the innermost lock acquisition around the final call.

Example:
```java
public class NotUnderLock {
  SettableFuture future = null;

  public void callFutureSetOk() {
    future.set(null);
  }

  public synchronized void firstAcquisitionBad() {
    callFutureSetOk();
  }

  public void secondAcquisitionOk(Object o) {
    synchronized (o) {
      firstAcquisitionBad();
    }
  }
}
```
