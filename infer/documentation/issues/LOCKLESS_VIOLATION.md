A method implements an interface signature annotated with `@Lockless` but which transitively acquires a lock.

Example:

```java
Interface I {
    @Lockless
    public void no_lock();
}

class C implements I {
  private synchronized do_lock() {}

  public void no_lock() { // this method should not acquire any locks
    do_lock();
  }
}
```
