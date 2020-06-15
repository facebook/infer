This warning indicates that Infer was not able to determine a static
upper bound on the execution cost of the procedure. By default, this
issue type is disabled.


For instance, Inferbo's interval analysis is limited to affine
expressions. Hence, we can't statically estimate an upper bound on the
below example and obtain T(unknown) cost:
```java
// Expected: square root(x), got T
void square_root_FP(int x) {
 int i = 0;
 while (i * i < x) {
   i++;
 }
}
```

Consequently, we report an `INFINITE_EXECUTION_TIME`, corresponding to the biggest bound T.
