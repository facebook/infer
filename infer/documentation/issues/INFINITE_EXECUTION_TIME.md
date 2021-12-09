This warning indicates that Infer was not able to determine a static
upper bound on the execution cost of the procedure. By default, this
issue type is disabled.

### Example 1: T due to expressivity

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
### Example 2: T due to unmodeled calls 
Another common case where we get T cost is when Infer cannot statically determine the range of values for loop bounds. For instance, 

```java
void loop_over_charArray_FP(StringBuilder builder, String input) {
  for (Character c : input.toCharArray()) {}
}
```
Here, Infer does not have any InferBo models for the range of values returned by `String.toCharArray`, hence it cannot determine that we will be iterating over a char array in the size of `input` string.  

To teach InferBo about such library calls, they should be semantically modeled in [InferBo](https://github.com/facebook/infer/blob/main/infer/src/bufferoverrun/bufferOverrunModels.ml).


### Example 3: T due to calling another T-costed function
Since the analysis is inter-procedural, another example we can have T cost is if at least one of the callees has T cost.

```java
// Expected: constant, got T
void call_top_cost_FP() {
 square_root_FP(1); // square_root_FP has Top cost
}
```

