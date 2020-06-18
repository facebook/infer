This is an experimental inter-procedural analysis that detects pure (side-effect free) functions. For each function, purity analysis keeps track of not only the purity of the function but also some additional information such as whether the function modifies a global variable or which of the parameters are modified. It models functions with no summary/model as modifying the global state (hence impure).

If the function is pure (i.e. doesn't modify any global state or its parameters and doesn't call any unknown functions), then it reports an [`PURE_FUNCTION`](/docs/next/all-issue-types#pure_function) issue.


## Weaknesses

There are two issues with the existing purity analysis:
- In order to detect which parameters are modified, we need an alias analysis which is difficult to get right.
- Just keeping track of modified arguments doesn't suffice.

Too see the issue with the first point, consider the following simple program:

```java
void foo(Foo a){
  Foo b = a;
  b.x = 10; 
}
```

in order to determine that `foo` is impure, we need to know that the write to `b`'s field is actually changing the function parameter `a`, i.e. we need to check if `b` is aliasing `a`. This is known as alias analysis and is hard to get right in a scalable manner. When this analysis was being developed, Infer didn't have a unified alias analysis and using biabduction seemed like a too daunting task at the time. Hence, we relied on [InferBo](/docs/next/checker-bufferoverrun)'s aliasing mechanism which was easy to invoke and integrate with. However, InferBo's aliasing analysis is far from perfect and causes issues for purity.
To see the issue with the second point, consider the following program:

```java
boolean contains(Integer i, ArrayList<Integer> list){
  Iterator<Integer> listIterator = list.iterator();
  while(listIterator.hasNext()) {
    Integer el = listIterator.next();
    if (i.equals(el)){
      return true;
    }
  }
  return false;
 }
```

The existing purity analysis concludes that the above function `contains` is impure because it calls an impure function `next()` which modifies the iterator (hence it thinks it also modifies the `list`). However, notice that `contains` doesn't have an observable side-effect: `list.iterator()` returns a new object, `hasNext()` and `equals()` are pure, and `next()` only modifies the fields of the fresh object `listIterator`.  Therefore, `contains` should be considered as pure.


To alleviate this problem, we have developed an [Impurity](/docs/next/checker-impurity) analysis which uses [pulse](/docs/next/checker-pulse) which can successfully analyze this program as pure \o/


The analysis is used by:

- [Loop-hoisting](/docs/next/checker-loop-hoisting) analysis which identifies loop-invariant function calls, i.e. functions that are pure and have loop-invariant arguments. 
- [Cost](/docs/next/checker-cost) analysis which identifies control variables in the loop that affect how many times a loop is executed. In this computation, we need to prune control variables that do not affect how many times a loop is executed. In this pruning step, we need to compute loop-invariant variables (which requires the above analysis).
