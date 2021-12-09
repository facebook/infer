\[EXPERIMENTAL\] This warning indicates that the procedure has non-constant and non-top execution cost. By default, this issue type is disabled. To enable it, set `enabled=true` in [costKind.ml](https://github.com/facebook/infer/blob/main/infer/src/base/costKind.ml#L55).

For instance, a simple example where we report this issue is a function with linear cost:
```java
int sum_linear(ArrayList<Integer> list){
 int sum = 0;
 for (Integer el: list){
   sum += el;
 }
 return sum;
}
```
