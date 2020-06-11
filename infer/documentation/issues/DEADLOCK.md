This error is currently reported in Java. A deadlock occurs when two distinct
threads try to acquire two locks in reverse orders. The following code
illustrates a textbook example. Of course, in real deadlocks, the lock
acquisitions may be separated by deeply nested call chains.

```java
  public void lockAThenB() {
    synchronized(lockA) {
      synchronized(lockB) {
       // do something with both resources
      }
    }
  }

  public void lockBThenA() {
    synchronized(lockB) {
      synchronized(lockA) {
       // do something with both resources
      }
    }
  }
```

The standard solution to a deadlock is to fix an order of lock acquisition and
adhere to that order in all cases. Another solution may be to shrink the
critical sections (i.e., the code executing under lock) to the minimum required.

Old-style containers such as `Vector` are synchronized on the object monitor,
which means that deadlocks can occur even without explicit synchronisation on
both threads. For instance:

```java
  public void lockAThenAddToVector() {
    synchronized(lockA) {
      vector.add(object);
    }
  }

  public void lockVectorThenA() {
    synchronized(vector) {
      synchronized(lockA) {
       // do something with both resources
      }
    }
  }
```

Infer has support for detecting these deadlocks too.

To suppress reports of deadlocks in a method `m()` use the
`@SuppressLint("DEADLOCK")` annotation, as follows:

```java
  import android.annotation.SuppressLint;

  @SuppressLint("DEADLOCK")
  public void m() {
  ...
  }
```
