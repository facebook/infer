We report this issue type when a function is [loop-invariant](/docs/next/all-issue-types#invariant_call) and also expensive (i.e. at least has linear complexity as determined by the [cost](/docs/next/checker-cost) analysis).

```java
int incr(int x) {
  return x + 1;
}

// incr will not be hoisted since it is cheap(constant time)
void foo_linear(int size) {
  int x = 10;
  for (int i = 0; i < size; i++) {
    incr(x); // constant call, don't hoist
  }
}

// call to foo_linear will be hoisted since it is expensive(linear in size).
void symbolic_expensive_hoist(int size) {
  for (int i = 0; i < size; i++) {
    foo_linear(size); // hoist
  }
}
```
