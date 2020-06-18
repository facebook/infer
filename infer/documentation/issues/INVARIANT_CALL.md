We report this issue type when a function call is loop-invariant and hoistable, i.e.
- the function has no side side effects (pure)
- has invariant arguments and result (i.e. have the same value in all loop iterations)
- it is guaranteed to execute, i.e. it dominates all loop sources

```java
int foo(int x, int y) {
 return x + y;
}


void invariant_hoist(int size) {
    int x = 10;
    int y = 5;
    for (int i = 0; i < size; i++) {
      foo(x, y); // hoistable
    }
  }
```
