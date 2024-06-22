---
title: List of all issue types
---

Here is an overview of the issue types currently reported by Infer.

## ARBITRARY_CODE_EXECUTION_UNDER_LOCK

*Reported as "Arbitrary Code Execution Under lock" by [starvation](/docs/checker-starvation).*

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

## BAD_ARG

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Bad Arg" by [pulse](/docs/checker-pulse).*

Bad arg in Erlang: Reports an error when the type of an argument is wrong or the argument is badly formed. Corresponds to the `badarg` error in the Erlang runtime.

For example, trying to concatenate the number `3` with  the list `[1,2]` gives `badarg` error because `3` is not a list.
```erlang
f() ->
    3 ++ [1,2]. // badarg error
```

Note that although the first argument needs to be a list, the second argument may not be a list.
For instance, concatenating [1,2] with the number `3` raises no error in Erlang.
```erlang
g() ->
    [1,2] ++ 3. // no error. Result: [1,2|3]
```

## BAD_ARG_LATENT

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Bad Arg Latent" by [pulse](/docs/checker-pulse).*

A latent [BAD_ARG](#bad_arg). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## BAD_GENERATOR

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Bad Generator" by [pulse](/docs/checker-pulse).*

Bad generator in Erlang: Reports an error when a wrong type is used in a generator. Corresponds to the `bad_generator` error in the Erlang runtime.

For example:
```erlang
list_instead_of_map() ->
    M = [],
    [{K, V} || K := V <- M]
```

## BAD_GENERATOR_LATENT

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Bad Generator Latent" by [pulse](/docs/checker-pulse).*

A latent [BAD_GENERATOR](#bad_generator). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## BAD_KEY

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Bad Key" by [pulse](/docs/checker-pulse).*

Bad key in Erlang: Reports an error when trying to access or update a non-existing key in a map. Corresponds to the `{badkey,K}` error in the Erlang runtime.

For example, trying to update the key `2` in `M` gives `{badkey,2}` error because `2` is not present as a key in `M`.
```erlang
f() ->
    M = #{},
    M#{2 := 3}.
```

Note that maps currently use a recency abstraction, meaning that only the most recent key/value is tracked.
Therefore, if a map is non-empty and we try to access a key other than the one we track, we just assume that it is there to avoid false positives.

## BAD_KEY_LATENT

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Bad Key Latent" by [pulse](/docs/checker-pulse).*

A latent [BAD_KEY](#bad_key). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## BAD_MAP

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Bad Map" by [pulse](/docs/checker-pulse).*

Bad map in Erlang: Reports an error when trying to access or update a key for a term that is not a map. Corresponds to the `{badmap,...}` error in the Erlang runtime.

For example, trying to update `L` as if it was a map gives `{badmap,[1,2,3]}` error because `L` is actually a list (`[1,2,3]`).
```erlang
f() ->
    L = [1,2,3],
    L#{1 => 2}.
```

## BAD_MAP_LATENT

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Bad Map Latent" by [pulse](/docs/checker-pulse).*

A latent [BAD_MAP](#bad_map). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## BAD_RECORD

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Bad Record" by [pulse](/docs/checker-pulse).*

Bad record in Erlang: Reports an error when trying to access or update a record with the wrong name. Corresponds to the `{badrecord,Name}` error in the Erlang runtime.

For example, accessing `R` as a `person` record gives `{badrecord,person}` error because `R` is `rabbit` (even though both share the `name` field).
```erlang
-record(person, {name, phone}).
-record(rabbit, {name, color}).

f() ->
    R = #rabbit{name = "Bunny", color = "Brown"},
    R#person.name.
```

## BAD_RECORD_LATENT

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Bad Record Latent" by [pulse](/docs/checker-pulse).*

A latent [BAD_RECORD](#bad_record). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## BAD_RETURN

*Reported as "Bad Return" by [pulse](/docs/checker-pulse).*

Bad return in Erlang: The dynamic type of a returned value disagrees with the static type given in the spec.

For example, this function returns an integer, while the spec says it returns an atom.
```erlang
-spec f() -> atom().
f() -> 1.
```

Note that this will *not* lead to a runtime error when running the Erlang program.

## BAD_RETURN_LATENT

*Reported as "Bad Return Latent" by [pulse](/docs/checker-pulse).*

A latent [BAD_RETURN](#bad_return). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## BIABDUCTION_MEMORY_LEAK

*Category: [Resource leak](/docs/all-categories#resource-leak). Reported as "Memory Leak" by [biabduction](/docs/checker-biabduction).*

See [MEMORY_LEAK_C](#memory_leak_c).
## BIABDUCTION_RETAIN_CYCLE

*Category: [Resource leak](/docs/all-categories#resource-leak). Reported as "Retain Cycle" by [biabduction](/docs/checker-biabduction).*

See [RETAIN_CYCLE](#retain_cycle).
## BLOCK_PARAMETER_NOT_NULL_CHECKED

*Reported as "Block Parameter Not Null Checked" by [parameter-not-null-checked](/docs/checker-parameter-not-null-checked).*

This error type is reported only in Objective-C/Objective-C++. It happens when a method has a block as a parameter,
and the block is executed in the method's body without checking it for `nil` first. If a `nil` block is passed to
the method, then this will cause a crash. For example:

```objectivec
- (void)uploadTaskWithRequest:(NSURLRequest*)urlRequest
                       fromFile:(NSURL*)fileURL
                       delegate:(id)delegate
                  delegateQueue:(NSOperationQueue*)delegateQueue
                     completion:(void (^)())completion {
     ...
    completion();
}
```

**Action**:
Possible solutions are adding a check for `nil`, or making sure that the method
is not ever called with `nil`. When an argument will never be `nil`, you can add
the annotation `nonnull` to the argument's type, to tell Infer (and the type
system), that the argument won't be `nil`. This will silence the warning.

## BUFFER_OVERRUN_L1

*Reported as "Buffer Overrun L1" by [bufferoverrun](/docs/checker-bufferoverrun).*

This is reported when outside of buffer bound is accessed.  It can corrupt memory and may introduce
security issues in C/C++.

For example, `int a[3]; a[5] = 42;` generates a `BUFFER_OVERRUN_L1` on `a[5] = 42;`.

Buffer overrun reports fall into several "buckets" corresponding to the expected precision of the
report.  The higher the number, the more likely it is to be a false positive.

*   `L1`: The most faithful report, when it *must* be unsafe.  For example, array size: `[3,3]`,
    offset: `[5,5]`.

*   `L2`: Less faithful report than `L1`, when it *may* be unsafe.  For example, array size:`[3,3]`,
    offset: `[0,5]`.  Note that the offset may be a safe value in the real execution, i.e. safe when
    0, 1, or 2; unsafe when 3, 4, or 5.

*   `L5`: The least faithful report, when there is an interval top.  For example, array size:
    `[3,3]`, offset: `[-oo,+oo]`.

*   `L4`: More faithful report than `L5`, when there is an infinity value.  For example, array size:
    `[3,3]`, offset: `[0, +oo]`.

*   `L3`: The reports that are not included in the above cases.

*   `S2`: An array access is unsafe by symbolic values.  For example, array size: `[n,n]`, offset
    `[n,+oo]`.

*   `U5`: An array access is unsafe by unknown values, which are usually from unknown function
    calls.

## BUFFER_OVERRUN_L2

*Reported as "Buffer Overrun L2" by [bufferoverrun](/docs/checker-bufferoverrun).*

See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)
## BUFFER_OVERRUN_L3

*Reported as "Buffer Overrun L3" by [bufferoverrun](/docs/checker-bufferoverrun).*

See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)
## BUFFER_OVERRUN_L4

*Reported as "Buffer Overrun L4" by [bufferoverrun](/docs/checker-bufferoverrun).*

See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)
## BUFFER_OVERRUN_L5

*Reported as "Buffer Overrun L5" by [bufferoverrun](/docs/checker-bufferoverrun).*

See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)
## BUFFER_OVERRUN_S2

*Reported as "Buffer Overrun S2" by [bufferoverrun](/docs/checker-bufferoverrun).*

See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)
## BUFFER_OVERRUN_U5

*Reported as "Buffer Overrun U5" by [bufferoverrun](/docs/checker-bufferoverrun).*

See [BUFFER_OVERRUN_L1](#buffer_overrun_l1)
## CAPTURED_STRONG_SELF

*Category: [Resource leak](/docs/all-categories#resource-leak). Reported as "Captured strongSelf" by [self-in-block](/docs/checker-self-in-block).*

This check is about when a strong pointer to `self` is captured in a block.
This could lead to retain cycles or unexpected behavior since to avoid retain
cycles one usually uses a local strong pointer or a captured weak pointer instead.

This will happen in one of two cases generally:

1. One uses `weakSelf` but forgot to declare it weak first.

Example:

```objectivec
  __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    return strongSelf->x;
  };
```

**Action:** Replace the first line with `__weak __typeof(self) weakSelf = self;`.


2. One is using `strongSelf`, declared in a block, in another inner block.
   The retain cycle is avoided in the outer block because `strongSelf` is a
   local variable of the block. If `strongSelf` is used in the inner block,
   then it's not a local variable anymore, but a captured variable.

   Example:

```objectivec
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong typeof(self) strongSelf = weakSelf;
    if (strongSelf) {
      int (^my_block)() = ^() {
        int x = strongSelf->x;
        ...
      };
      ...
    }
    ...
  };
```

In this example, `strongSelf` is a captured variable of the inner block, and this could cause retain cycles.

**Action:** Use a new pointer to self local to the inner block. In the example:

```objectivec
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong typeof(self) strongSelf = weakSelf;
    if (strongSelf) {
      int (^my_block)() = ^() {
         __typeof(self) innerStrongSelf = weakSelf;
        int x = innerStrongSelf->x;
        ...
      };
      ...
    }
    ...
  };
```

Or, to improve readability, move the inner block logic into a separate method.

Another solution could be to copy the instance variable that one needs to access inside the inner block to a local variable, and use the local variable instead:

```objectivec
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong typeof(self) strongSelf = weakSelf;
    if (strongSelf) {
      int my_x = strongSelf->x;
      int (^my_block)() = ^() {
        int x = my_x;
        ...
      };
      ...
    }
    ...
  };
```

## CHECKERS_ALLOCATES_MEMORY

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Allocates Memory" by [annotation-reachability](/docs/checker-annotation-reachability).*

A method annotated with `@NoAllocation` transitively calls `new`.

Example:

```java
class C implements I {
  @NoAllocation
  void directlyAllocatingMethod() {
    new Object();
  }
}
```

## CHECKERS_ANNOTATION_REACHABILITY_ERROR

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Annotation Reachability Error" by [annotation-reachability](/docs/checker-annotation-reachability).*

A method annotated with an annotation `@A` transitively calls a method annotated `@B` where the combination of annotations is forbidden (for example, `@UiThread` calling `@WorkerThread`).

## CHECKERS_CALLS_EXPENSIVE_METHOD

*Reported as "Expensive Method Called" by [annotation-reachability](/docs/checker-annotation-reachability).*

A method annotated with `@PerformanceCritical` transitively calls a method annotated `@Expensive`.

Example:

```java
class C {
  @PerformanceCritical
  void perfCritical() {
    expensive();
  }

  @Expensive
  void expensive() {}
}
```

## CHECKERS_EXPENSIVE_OVERRIDES_UNANNOTATED

*Reported as "Expensive Overrides Unannotated" by [annotation-reachability](/docs/checker-annotation-reachability).*

A method annotated with `@Expensive` overrides an un-annotated method.

Example:

```java
interface I {
  void foo();
}

class A implements I {
  @Expensive
  public void foo() {}
}
```

## CHECKERS_FRAGMENT_RETAINS_VIEW

*Category: [Resource leak](/docs/all-categories#resource-leak). Reported as "Fragment Retains View" by [fragment-retains-view](/docs/checker-fragment-retains-view).*

This error type is Android-specific. It fires when a `Fragment` type fails to
nullify one or more of its declared `View` fields in `onDestroyView`. In
performance-sensitive applications, a `Fragment` should initialize all `View`'s
in `onCreateView` and nullify them in `onDestroyView`. If a `Fragment` is placed
on the back stack and fails to nullify a `View` in `onDestroyView`, it will
retain a useless reference to that `View` that will not be cleaned up until the
`Fragment` is resumed or destroyed.

Action: Nullify the `View` in question in `onDestroyView`.

## CHECKERS_PRINTF_ARGS

*Reported as "Printf Args" by [printf-args](/docs/checker-printf-args).*

This error is reported when the argument types to a `printf` method do not match the format string.

```java
  void stringInsteadOfInteger(PrintStream out) {
    out.printf("Hello %d", "world");
  }
```

Action: fix the mismatch between format string and argument types.

## CONFIG_IMPACT

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Config Impact" by [config-impact-analysis](/docs/checker-config-impact-analysis).*

Infer reports this issue when an *expensive* function is called without a *config check*.  The
*config* is usually a boolean value that enables experimental new features and it is defined per
application/codebase, e.g. gatekeepers.  To determine whether a function is expensive or not, the
checker relies on modeled functions that are assumed to be expensive, e.g. string operations,
regular expression match, or DB accesses.

Similar to [Cost analysis](/docs/checker-cost), this issue type is reported only in
differential mode, i.e. when there are original code and modified one and we can compare Infer's
results on both of them.

For instance, if we have the following code

```java
// version1
foo();
if (config_check){
   bar();
}
```

which is then modified to next

```java
// version2
foo();
if (config_check){
   bar();
}
goo(); // added
```

the analysis would warn the developer that "`goo()` is a newly added function call and it might
cause an unexpected new behavior". However, if we were to add `goo()` right after `bar()`, then
Infer wouldn't warn about it because it is already gated under the `config_check`.

The analysis is inter-procedural: it can reason about impacts by code changes not only inside a
single procedure, but also the impacts that are propagated by function calls. Thus, if we were to
modify `version1` to `version3` below by calling `goo()` in `foo()`,

```java
// version3
void foo(){
   // ....
   goo(); // added
}
```

then the analysis will report a `CONFIG_IMPACT` issue on the ungated call site of `foo()`.

Currently, the analysis supports both Objective-C and Java but not C++.

Action: Make sure the ungated code change is semantically correct and harmless in terms of execution
cost.  If you are not sure, gate it with a new or pre-existing config.

## CONFIG_IMPACT_STRICT

*Category: [Ungated code](/docs/all-categories#ungated-code). Reported as "Config Impact Strict" by [config-impact-analysis](/docs/checker-config-impact-analysis).*

This is similar to [`CONFIG_IMPACT` issue](#config_impact) but the analysis reports **all** ungated
codes irrespective of whether they are expensive or not.

## CONFIG_USAGE

*Reported as "Config Usage" by [pulse](/docs/checker-pulse).*

Infer reports this issue when a *config* value is used as branch condition in a function.  The
*config* is usually a boolean value that enables experimental new features and it is defined per
application/codebase, e.g. gatekeepers.

For instance, if we have the following code

```cpp
void foo() {
  if(config_check("my_new_feature")){ ... }
}
```

then analysis would provide information that "the function `foo` uses the config `my_new_feature` as
branch condition".

Note: This type of issue is only for providing semantic information, rather than warning or
reporting actual problem.

## CONSTANT_ADDRESS_DEREFERENCE

*Reported as "Constant Address Dereference" by [pulse](/docs/checker-pulse).*

This is reported when an address at an absolute location, e.g. 1234,
is dereferenced. It is a more general version of the
[`NULLPTR_DEREFERENCE`](#nullptr_dereference) error type that is
reported when the address is a constant other than zero.

For example, `int *p = (int *) 123; *p = 42;` generates a `CONSTANT_ADDRESS_DEREFERENCE` on `*p = 42;`.

For more information see the [`NULLPTR_DEREFERENCE`](#nullptr_dereference) issue type.

## CONSTANT_ADDRESS_DEREFERENCE_LATENT

*Reported as "Constant Address Dereference Latent" by [pulse](/docs/checker-pulse).*

A latent [CONSTANT_ADDRESS_DEREFERENCE](#constant_address_dereference). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## CREATE_INTENT_FROM_URI

*Reported as "Create Intent From Uri" by [quandary](/docs/checker-quandary).*

Create an intent/start a component using a (possibly user-controlled) URI. may or may not be an issue depending on where the URI comes from.
## CROSS_SITE_SCRIPTING

*Reported as "Cross Site Scripting" by [quandary](/docs/checker-quandary).*

Untrusted data flows into HTML; XSS risk.
## CXX_REF_CAPTURED_IN_BLOCK

*Category: [Memory error](/docs/all-categories#memory-error). Reported as "C++ Reference Captured in Block" by [self-in-block](/docs/checker-self-in-block).*

This check flags when a C++ reference is captured in an escaping block.
This means that the block will be leaving the current scope, i.e. it is
not annotated with `__attribute__((noescape))`.

Example:

```
- (void)ref_captured_in_escaping_block_bad:(int&)y {
  dispatch_async(dispatch_get_main_queue(), ^{
    int a = y;
    ...
  });
  ...;
}
```

This could cause crashes because C++ references are not managed pointers
(like ARC pointers) and so the referent is likely to be gone if the block
dereferences it later.

## DANGLING_POINTER_DEREFERENCE

*Reported as "Dangling Pointer Dereference" by [biabduction](/docs/checker-biabduction).*


## DATALOG_FACT

*Reported as "Datalog Fact" by [datalog](/docs/checker-datalog).*

Datalog fact used as input for a datalog solver.
## DATA_FLOW_TO_SINK

*Category: [Sensitive data flow](/docs/all-categories#sensitive-data-flow). Reported as "Data Flow to Sink" by [pulse](/docs/checker-pulse).*

A flow of data was detected to a sink.
## DEADLOCK

*Category: [Concurrency](/docs/all-categories#concurrency). Reported as "Deadlock" by [starvation](/docs/checker-starvation).*

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

## DEAD_STORE

*Category: [Logic error](/docs/all-categories#logic-error). Reported as "Dead Store" by [liveness](/docs/checker-liveness).*

This error is reported in C++. It fires when the value assigned to a variables
is never used (e.g., `int i = 1; i = 2; return i;`).

## DIVIDE_BY_ZERO

*Reported as "Divide By Zero" by [biabduction](/docs/checker-biabduction).*


## EMPTY_VECTOR_ACCESS

*Reported as "Empty Vector Access" by [biabduction](/docs/checker-biabduction).*

This error type is reported only in C++, in versions >= C++11.

The code is trying to access an element of a vector that Infer believes to be
empty. Such an access will cause undefined behavior at runtime.

```cpp
#include <vector>
int foo(){
  const std::vector<int> vec;
  return vec[0]; // Empty vector access reported here
}
```

## EXECUTION_TIME_COMPLEXITY_INCREASE

*Reported as "Execution Time Complexity Increase" by [cost](/docs/checker-cost).*

Infer reports this issue when the execution time complexity of a
program increases in degree: e.g. from constant to linear or from
logarithmic to quadratic. This issue type is only reported in
differential mode: i.e when we are comparing the cost analysis results of
two runs of infer on a file. Check out examples in [here](/docs/checker-cost#examples-execution-cost).



## EXECUTION_TIME_COMPLEXITY_INCREASE_UI_THREAD

*Reported as "Execution Time Complexity Increase Ui Thread" by [cost](/docs/checker-cost).*

Infer reports this issue when the execution time complexity of the procedure increases in degree **and** the procedure runs on the UI (main) thread.

Infer considers a method as running on the UI thread whenever:

- The method, one of its overrides, its class, or an ancestral class, is
  annotated with `@UiThread`.
- The method, or one of its overrides is annotated with `@OnEvent`, `@OnClick`,
  etc.
- The method or its callees call a `Litho.ThreadUtils` method such as
  `assertMainThread`.


## EXECUTION_TIME_UNREACHABLE_AT_EXIT

*Reported as "Execution Time Unreachable At Exit" by [cost](/docs/checker-cost).*

This issue type indicates that the program's execution doesn't reach
the exit node (where our analysis computes the final cost of the
procedure). Hence, we cannot compute a static bound for the procedure.


Examples:
```java
void exit_unreachable() {
  exit(0); // modeled as unreachable
}

void infeasible_path_unreachable() {
    Preconditions.checkState(false); // like assert false, state pruned to bottom
}
```

## EXPENSIVE_EXECUTION_TIME

*Reported as "Expensive Execution Time" by [cost](/docs/checker-cost).*

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

## EXPENSIVE_LOOP_INVARIANT_CALL

*Reported as "Expensive Loop Invariant Call" by [loop-hoisting](/docs/checker-loop-hoisting).*

We report this issue type when a function is [loop-invariant](/docs/all-issue-types#invariant_call) and also expensive (i.e. at least has linear complexity as determined by the [cost](/docs/checker-cost) analysis).

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

## EXPOSED_INSECURE_INTENT_HANDLING

*Reported as "Exposed Insecure Intent Handling" by [quandary](/docs/checker-quandary).*

Undocumented.
## GUARDEDBY_VIOLATION

*Category: [Concurrency](/docs/all-categories#concurrency). Reported as "GuardedBy Violation" by [racerd](/docs/checker-racerd).*

A field annotated with `@GuardedBy` is being accessed by a call-chain that starts at a non-private method without synchronization.

Example:

```java
class C {
  @GuardedBy("this")
  String f;

  void foo(String s) {
    f = s; // unprotected access here
  }
}
```

Action: Protect the offending access by acquiring the lock indicated by the `@GuardedBy(...)`.

## IMPURE_FUNCTION

*Reported as "Impure Function" by [impurity](/docs/checker-impurity).*

This issue type indicates impure functions. For instance, below functions would be marked as impure:
```java
void makeAllZero_impure(ArrayList<Foo> list) {
  Iterator<Foo> listIterator = list.iterator();
  while (listIterator.hasNext()) {
    Foo foo = listIterator.next();
    foo.x = 0;
  }
}
```

## INEFFICIENT_KEYSET_ITERATOR

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Inefficient Keyset Iterator" by [inefficient-keyset-iterator](/docs/checker-inefficient-keyset-iterator).*

This issue is raised when
- iterating over a HashMap with `keySet()` iterator
- looking up the key each time

Example:

```java
void inefficient_loop_bad(HashMap<String, Integer> testMap) {
 for (String key : testMap.keySet()) {
   Integer value = testMap.get(key); // extra look-up cost
   foo(key, value);
 }
}
```

**Action**:

Instead, it is more efficient to iterate over the loop with `entrySet` which returns key-vaue pairs and gets rid of the hashMap lookup.
 
```java
void efficient_loop_ok(HashMap<String, Integer> testMap) {
  for (Map.Entry<String, Integer> entry : testMap.entrySet()) {
    String key = entry.getKey();
    Integer value = entry.getValue();
    foo(key, value);
  }
}
```

## INFERBO_ALLOC_IS_BIG

*Reported as "Alloc Is Big" by [bufferoverrun](/docs/checker-bufferoverrun).*

`malloc` is passed a large constant value (>=10^6). For example, `int n = 1000000; malloc(n);` generates `INFERBO_ALLOC_IS_BIG` on `malloc(n)`.

Action: Fix the size argument or make sure it is really needed.
## INFERBO_ALLOC_IS_NEGATIVE

*Reported as "Alloc Is Negative" by [bufferoverrun](/docs/checker-bufferoverrun).*

`malloc` is called with a negative size. For example, `int n = 3 - 5; malloc(n);` generates `INFERBO_ALLOC_IS_NEGATIVE` on `malloc(n)`.

Action: Fix the size argument.
## INFERBO_ALLOC_IS_ZERO

*Reported as "Alloc Is Zero" by [bufferoverrun](/docs/checker-bufferoverrun).*

`malloc` is called with a zero size. For example, `int n = 3 - 3; malloc(n);` generates `INFERBO_ALLOC_IS_ZERO` on `malloc(n)`.

Action: Fix the size argument.
## INFERBO_ALLOC_MAY_BE_BIG

*Reported as "Alloc May Be Big" by [bufferoverrun](/docs/checker-bufferoverrun).*

`malloc` *may* be called with a large value. For example, `int n = b ? 3 : 1000000; malloc(n);` generates `INFERBO_ALLOC_MAY_BE_BIG` on `malloc(n)`.

Action: Fix the size argument or add a bound checking, e.g. `if (n < A_SMALL_NUMBER) { malloc(n); }`.
## INFERBO_ALLOC_MAY_BE_NEGATIVE

*Reported as "Alloc May Be Negative" by [bufferoverrun](/docs/checker-bufferoverrun).*

`malloc` *may* be called with a negative value. For example, `int n = b ? 3 : -5; malloc(n);` generates `INFERBO_ALLOC_MAY_BE_NEGATIVE` on `malloc(n)`.

Action: Fix the size argument or add a bound checking, e.g. `if (n > 0) { malloc(n); }`.
## INFINITE_EXECUTION_TIME

*Reported as "Infinite Execution Time" by [cost](/docs/checker-cost).*

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


## INSECURE_INTENT_HANDLING

*Reported as "Insecure Intent Handling" by [quandary](/docs/checker-quandary).*

Undocumented.
## INTEGER_OVERFLOW_L1

*Reported as "Integer Overflow L1" by [bufferoverrun](/docs/checker-bufferoverrun).*

This is reported when integer overflow occurred by integer operations such as addition, subtraction,
and multiplication. For example, `int n = INT_MAX; int m = n + 3;` generates a INTEGER_OVERFLOW_L1
on `n + 3`.

Integer overflows reports fall into several "buckets" corresponding to the expected precision of the
report. The higher the number, the more likely it is to be a false positive.

*   `L1`: The most faithful report, when it *must* be unsafe.  For example,
    `[2147483647,2147483647] + [1,1]` in 32-bit signed integer type.

*   `L2`: Less faithful report than `L1`, when it *may* be unsafe.  For example,
    `[2147483647,2147483647] + [0,1]` in 32-bit signed integer type.  Note that the integer of RHS
    can be 0, which is safe.

*   `L5`: The reports that are not included in the above cases.

*   `U5`: A binary integer operation is unsafe by unknown values, which are usually from unknown
    function calls.

## INTEGER_OVERFLOW_L2

*Reported as "Integer Overflow L2" by [bufferoverrun](/docs/checker-bufferoverrun).*

See [INTEGER_OVERFLOW_L1](#integer_overflow_l1)
## INTEGER_OVERFLOW_L5

*Reported as "Integer Overflow L5" by [bufferoverrun](/docs/checker-bufferoverrun).*

See [INTEGER_OVERFLOW_L1](#integer_overflow_l1)
## INTEGER_OVERFLOW_U5

*Reported as "Integer Overflow U5" by [bufferoverrun](/docs/checker-bufferoverrun).*

See [INTEGER_OVERFLOW_L1](#integer_overflow_l1)
## INTERFACE_NOT_THREAD_SAFE

*Category: [Concurrency](/docs/all-categories#concurrency). Reported as "Interface Not Thread Safe" by [racerd](/docs/checker-racerd).*

This error indicates that you have invoked an interface method not annotated
with `@ThreadSafe` from a thread-safe context (e.g., code that uses locks or is
marked `@ThreadSafe`). The fix is to add the `@ThreadSafe` annotation to the
interface or to the interface method. For background on why these annotations
are needed, see the detailed explanation
[here](/docs/checker-racerd#interface-not-thread-safe).

## INVALID_SIL

*Reported as "Invalid Sil" by [sil-validation](/docs/checker-sil-validation).*

The SIL instruction does not conform to the expected subset of instructions
expected for the front-end of the language for the analyzed code.

## INVARIANT_CALL

*Reported as "Invariant Call" by [loop-hoisting](/docs/checker-loop-hoisting).*

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

## IPC_ON_UI_THREAD

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Ipc On Ui Thread" by [starvation](/docs/checker-starvation).*

A blocking `Binder` IPC call occurs on the UI thread.
## JAVASCRIPT_INJECTION

*Reported as "Javascript Injection" by [quandary](/docs/checker-quandary).*

Untrusted data flows into JavaScript.
## LAB_RESOURCE_LEAK

*Reported as "Lab Resource Leak" by [resource-leak-lab](/docs/checker-resource-leak-lab).*

Toy issue.
## LOCKLESS_VIOLATION

*Reported as "Lockless Violation" by [starvation](/docs/checker-starvation).*

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

## LOCK_CONSISTENCY_VIOLATION

*Category: [Concurrency](/docs/all-categories#concurrency). Reported as "Lock Consistency Violation" by [racerd](/docs/checker-racerd).*

This is an error reported on C++ and Objective C classes whenever:

- Some class method directly uses locking primitives (not transitively).
- It has a public method which writes to some member `x` while holding a lock.
- It has a public method which reads `x` without holding a lock.

The above may happen through a chain of calls. Above, `x` may also be a
container (an array, a vector, etc).

### Fixing Lock Consistency Violation reports

- Avoid the offending access (most often the read). Of course, this may not be
  possible.
- Use synchronization to protect the read, by using the same lock protecting the
  corresponding write.
- Make the method doing the read access private. This should silence the
  warning, since Infer looks for a pair of non-private methods. Objective-C:
  Infer considers a method as private if it's not exported in the header-file
  interface.

## LOGGING_PRIVATE_DATA

*Reported as "Logging Private Data" by [quandary](/docs/checker-quandary).*

Undocumented.
## MEMORY_LEAK_C

*Category: [Resource leak](/docs/all-categories#resource-leak). Reported as "Memory Leak" by [pulse](/docs/checker-pulse).*

### Memory leak in C

This error type is only reported in C and Objective-C code. In Java we do not
report memory leaks because it is a garbage collected language.

In C, Infer reports memory leaks when objects are created with `malloc` and not
freed. For example:

```c
-(void) memory_leak_bug {
    struct Person *p = malloc(sizeof(struct Person));
}
```

### Memory leak in Objective-C

Additionally, in Objective-C, Infer reports memory leaks that happen when
objects from Core Foundation or Core Graphics don't get released.

```objectivec
-(void) memory_leak_bug_cf {
    CGPathRef shadowPath = CGPathCreateWithRect(self.inputView.bounds, NULL); //object created and not released.
}
```

## MEMORY_LEAK_CPP

*Category: [Resource leak](/docs/all-categories#resource-leak). Reported as "Memory Leak" by [pulse](/docs/checker-pulse).*

See [MEMORY_LEAK_C](#memory_leak_c)
## MISSING_REQUIRED_PROP

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Missing Required Prop" by [litho-required-props](/docs/checker-litho-required-props).*

This issues is reported when a required `@Prop` is missing.


## Examples

Assume that the following Litho Component specification is defined as follows where `prop1` is optional and `prop2` is required.

```java
class MyComponentSpec {

  static void onCreate(
      ComponentContext c,
      @Prop(optional = true) String prop1, @Prop int prop2) {
    ...
  }
  ...
}
```

When we build the corresponding component, we should have all the required props. If we are missing optional props (e..g `prop1` below), it is ok.

```java
MyComponent.create(c)
    .prop2(8)
    .build();
```

However, if we are missing a required prop, Infer gives an error below for the missing `prop2`.

```java
MyComponent.create(c)
    .prop1("My prop 1")
    .build();
```

** Action **

There are two ways to fix this issue.

First, we could add the missing `prop2`:

```java
MyComponent.create(c)
    .prop1("My prop 1")
    .prop2(x) // where x is some integer
    .build();
```

or alternatively, if the `prop2` is not really required, we could change the component spec to reflect that:


```java
class MyComponentSpec {

  static void onCreate(
      ComponentContext c,
      @Prop(optional = true) String prop1, @Prop(optional = true) int prop2) {
    ...
  }
  ...
}
```
## MIXED_SELF_WEAKSELF

*Category: [Resource leak](/docs/all-categories#resource-leak). Reported as "Mixed Self WeakSelf" by [self-in-block](/docs/checker-self-in-block).*

This check reports an issue when an Objective-C block captures both `self` and `weakSelf`, a weak pointer to `self`.
Possibly the developer meant to capture only `weakSelf` to avoid a retain cycle, but made a typo and used `self`
instead of `strongSelf`. In this case, this could cause a retain cycle.

Example:

```objectivec
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf) {
      [strongSelf foo];
      int x = self->x; // typo here
    }
    return 0;
  };
```

**Action**: Fixing the typo is generally the right course of action.

*Limitations:* To keep this check simple and intra-procedural, we rely on names to find `weakSelf`:
we assume that any captured weak pointer whose name contains "self" is a weak reference to `self`.

## MODIFIES_IMMUTABLE

*Reported as "Modifies Immutable" by [impurity](/docs/checker-impurity).*

This issue type indicates modifications to fields marked as @Immutable. For instance, below function `mutateArray` would be marked as modifying immutable field `testArray`:
```java
  @Immutable int[] testArray = new int[]{0, 1, 2, 4};
  
  int[] getTestArray() {
    return testArray;
  }                
          
  void mutateArray() {
    int[] array = getTestArray();
    array[2] = 7;
  }
```

## MULTIPLE_WEAKSELF

*Reported as "Multiple WeakSelf Use" by [self-in-block](/docs/checker-self-in-block).*

This check reports when an Objective-C block uses `weakSelf` (a weak pointer to `self`) more than once.
This could lead to unexpected behaviour. Even if `weakSelf` is not nil in the first use, it could be nil
in the following uses since the object that `weakSelf` points to could be freed anytime.

Example:

```objectivec
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
      [weakSelf foo];
      int x = weakSelf->x;
  };
```

**Action:**
One should assign `weakSelf` to a strong pointer first, and then
use it in the block.

```objectivec
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf) {
      [strongSelf foo];
      int x = strongSelf->x;
    }
    ...
  };
```

*Limitations:* To keep this check simple and intra-procedural, we rely on names to find `weakSelf`:
we assume that any captured weak pointer whose name contains "self" is a weak reference to `self`.
In contrast, `strongSelf` is a local variable to the block, so the check supports any name given to
a local strong pointer that has been assigned `weakSelf`.

## MUTUAL_RECURSION_CYCLE

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Mutual Recursion Cycle" by [pulse](/docs/checker-pulse).*

A recursive call or mutually recursive call has been detected. This does *not* mean that the program won't terminate, just that the code is recursive. You should double-check if the recursion is intended and if it can lead to non-termination or a stack overflow.

Example of recursive function:


```C
int factorial(int x) {
  if (x > 0) {
    return x * factorial(x-1);
  } else {
    return 1;
  }
}
```

## NIL_BLOCK_CALL

*Category: [Null pointer dereference](/docs/all-categories#null-pointer-dereference). Reported as "Nil Block Call" by [pulse](/docs/checker-pulse).*

This check reports when one tries to call an Objective-C block that is `nil`.
This causes a crash.

Example:

```objectivec
-(void) foo:(void (^)())callback {
    callback();
}

-(void) bar {
    [self foo:nil]; //crash
}
```

**Action**:

Adding a check for `nil` before calling the block, or making sure never to call the method `foo:` with `nil`.

## NIL_BLOCK_CALL_LATENT

*Category: [Null pointer dereference](/docs/all-categories#null-pointer-dereference). Reported as "Nil Block Call Latent" by [pulse](/docs/checker-pulse).*

A latent [NIL_BLOCK_CALL](#nil_block_call). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## NIL_INSERTION_INTO_COLLECTION

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Nil Insertion Into Collection" by [pulse](/docs/checker-pulse).*

This checks reports when `nil` is passed to collections in Objective-C such as arrays and dictionaries. This causes a crash.

### Arrays

Adding objects to an array, inserting objects at a given index, or replacing objects at a given index, can all
lead to a crash when the object is `nil`.

```objectivec
  [mArray addObject:nil];  //crash

  [mArray insertObject:nil atIndex:0];   //crash

  [mArray replaceObjectAtIndex:0 withObject:nil]; //crash
```

### Dictionaries

Adding a `nil` value in a dictionary causes a crash. If the concept of `nil` is required, one can add
`[NSNull null]` instead.

```objectivec
  id value = nil;
  [mDict setObject:value forKey:@"somestring"]; //crash

  [mDict setObject:[NSNull null] forKey:@"somestring"]; //ok
```

Retrieving or removing an object from a dictionary with a `nil` key also causes a crash:

```objectivec
    id key = nil;
    mDict[key] = @"somestring"; //crash

   [mDict removeObjectForKey:nil]; //crash
```

**Action**:

In all the cases above, when passing `nil` causes a crash, the solutions are either making sure
that the object passed will never be `nil`, or adding a check for `nil` before calling those methods.

## NIL_INSERTION_INTO_COLLECTION_LATENT

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Nil Insertion Into Collection" by [pulse](/docs/checker-pulse).*

A latent [NIL_INSERTION_INTO_COLLECTION](#nil_insertion_into_collection). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## NIL_MESSAGING_TO_NON_POD

*Category: [Memory error](/docs/all-categories#memory-error). Reported as "Nil Messaging To Non Pod" by [pulse](/docs/checker-pulse).*

In Objective-C, calling a method on `nil` (or in Objective-C terms, sending a message to `nil`) does not crash,
it simply returns a falsy value (nil/0/false). However, sending a message that returns
a non-POD C++ type (POD being ["Plain Old Data"](https://en.cppreference.com/w/cpp/named_req/PODType), essentially
anything that cannot be compiled as a C-style struct) to `nil` causes undefined behaviour.

```objectivec
std::shared_ptr<int> callMethodReturnsnonPOD() {
  SomeObject* obj = getObjectOrNil();
  std::shared_ptr<int> d = [obj returnsnonPOD]; // UB
  return d;
}
```

To fix the above issue, we need to check if `obj` is
not `nil` before calling the `returnsnonPOD` method:

```objectivec
std::shared_ptr<int> callMethodReturnsnonPOD(bool b) {
  SomeObject* obj = getObjectOrNil(b);
  if (obj == nil) { return std::make_shared<int>(0); }
  std::shared_ptr<int> d = [obj returnsnonPOD];
  return d;
}
```

## NIL_MESSAGING_TO_NON_POD_LATENT

*Category: [Memory error](/docs/all-categories#memory-error). Reported as "Nil Messaging To Non Pod Latent" by [pulse](/docs/checker-pulse).*

A latent [NIL_MESSAGING_TO_NON_POD](#nil_messaging_to_non_pod). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## NO_MATCHING_BRANCH_IN_TRY

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "No Matching Branch In Try" by [pulse](/docs/checker-pulse).*

No matching branch is found when evaluating the `of` section of a `try` expression. Corresponds to the `{try_clause,V}` error in the Erlang runtime.

For example, if we call `tail([])` and the full definition of `tail` is
```erlang
tail(X) ->
    try X of
        [_|T] -> {ok,T}
    catch
        _ -> error
    end.
```

## NO_MATCHING_BRANCH_IN_TRY_LATENT

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "No Matching Branch In Try Latent" by [pulse](/docs/checker-pulse).*

A latent [NO_MATCHING_BRANCH_IN_TRY](#no_matching_branch_in_try). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## NO_MATCHING_CASE_CLAUSE

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "No Matching Case Clause" by [pulse](/docs/checker-pulse).*

No matching case clause in Erlang: Reports an error when none of the clauses of a `case` match the expression. Corresponds to the `{case_clause,V}` error in the Erlang runtime.

For example, if we call `tail([])` and the full definition of `tail` is
```erlang
tail(X) ->
    case X of
        [_|T] -> T
    end.
```

This error is reported if either the pattern(s) or the guard(s) prevent matching any of the clauses.

## NO_MATCHING_CASE_CLAUSE_LATENT

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "No Matching Case Clause Latent" by [pulse](/docs/checker-pulse).*

A latent [NO_MATCHING_CASE_CLAUSE](#no_matching_case_clause). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## NO_MATCHING_ELSE_CLAUSE

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "No Matching Else Clause" by [pulse](/docs/checker-pulse).*

No matching else clause in Erlang: Reports an error when none of the clauses of an `else` match the short-circuit result from `maybe` body. Corresponds to the `{else_clause,V}` error in the Erlang runtime.

For example, here the `1 ?= 2` expression does not match and short-circuits to `2`, which does not match the single clause under `else`:
```erlang
else_clause_error() ->
    maybe
        1 ?= 2
    else
        1 -> ok
    end.
```

This error is reported if either the pattern(s) or the guard(s) prevent matching any of the clauses.

## NO_MATCHING_ELSE_CLAUSE_LATENT

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "No Matching Else Clause Latent" by [pulse](/docs/checker-pulse).*

A latent [NO_MATCHING_ELSE_CLAUSE](#no_matching_else_clause). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## NO_MATCHING_FUNCTION_CLAUSE

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "No Matching Function Clause" by [pulse](/docs/checker-pulse).*

No matching function clause in Erlang: Reports an error when none of the clauses of a function match the arguments of a call. Corresponds to the `function_clause` error in the Erlang runtime.

For example, if we call `tail([])` and the full definition of `tail` is
```erlang
tail([_|Xs]) -> Xs.
```

This error is reported if either the pattern(s) or the guard(s) prevent matching any of the clauses.

## NO_MATCHING_FUNCTION_CLAUSE_LATENT

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "No Matching Function Clause Latent" by [pulse](/docs/checker-pulse).*

A latent [NO_MATCHING_FUNCTION_CLAUSE](#no_matching_function_clause). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## NO_MATCH_OF_RHS

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "No Match Of Rhs" by [pulse](/docs/checker-pulse).*

No match of right hand side value in Erlang: Reports an error when the right hand side value of a `match` expression does not match the pattern on the left hand side. Corresponds to the `{badmatch,V}` error in the Erlang runtime.

For example, `[H|T] = []` gives the error because the left hand side pattern requires at least one element in the list on the right hand side.

## NO_MATCH_OF_RHS_LATENT

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "No Match Of Rhs Latent" by [pulse](/docs/checker-pulse).*

A latent [NO_MATCH_OF_RHS](#no_match_of_rhs). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## NO_TRUE_BRANCH_IN_IF

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "No True Branch In If" by [pulse](/docs/checker-pulse).*

No true branch when evaluating an if expression in Erlang: Reports an error when none of the branches of an `if` expression evaluate to true. Corresponds to the `if_clause` error in the Erlang runtime.

For example, if we call `sign(0)` and the full definition of `sign` is
```erlang
sign(X) ->
    if
        X > 0 -> positive;
        X < 0 -> negative
    end.
```

## NO_TRUE_BRANCH_IN_IF_LATENT

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "No True Branch In If Latent" by [pulse](/docs/checker-pulse).*

A latent [NO_TRUE_BRANCH_IN_IF](#no_true_branch_in_if). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## NULLPTR_DEREFERENCE

*Category: [Null pointer dereference](/docs/all-categories#null-pointer-dereference). Reported as "Null Dereference" by [pulse](/docs/checker-pulse).*

Infer reports null dereference bugs in Java, C, C++, and Objective-C
when it is possible that the null pointer is dereferenced, leading to
a crash.

### Null dereference in Java

Many of Infer's reports of potential Null Pointer Exceptions (NPE) come from code of the form

```java
  p = foo(); // foo() might return null
  stuff();
  p.goo();   // dereferencing p, potential NPE
```

If you see code of this form, then you have several options.

**If you are unsure whether or not `foo()` will return null**, you should
ideally either

1. Change the code to ensure that `foo()` can not return null, or

2. Add a check that `p` is not `null` before dereferencing `p`.

Sometimes, in case (2) it is not obvious what you should do when `p`
is `null`. One possibility is to throw an exception, failing early but
explicitly. This can be done using `checkNotNull` as in the following
code:

```java
// code idiom for failing early
import static com.google.common.base.Preconditions.checkNotNull;

  //... intervening code

  p = checkNotNull(foo()); // foo() might return null
  stuff();
  p.goo(); // p cannot be null here
```

The call `checkNotNull(foo())` will never return `null`: if `foo()`
returns `null` then it fails early by throwing a Null Pointer
Exception.

Facebook NOTE: **If you are absolutely sure that foo() will not be
null**, then if you land your diff this case will no longer be
reported after your diff makes it to trunk.

### Null dereference in C

Here is an example of an inter-procedural null dereference bug in C:

```c
struct Person {
  int age;
  int height;
  int weight;
};
int get_age(struct Person *who) {
  return who->age;
}
int null_pointer_interproc() {
  struct Person *joe = 0;
  return get_age(joe);
}
```

### Null dereference in Objective-C

In Objective-C, null dereferences are less common than in Java, but they still
happen and their cause can be hidden. In general, passing a message to nil does
not cause a crash and returns `nil`, but dereferencing a pointer directly does
cause a crash.

Example:

```objectivec
(int) foo:(C*) param {  // passing nil
  D* d = [param bar];   // nil message passing
  return d->fld;        // crash
}
(void) callFoo {
  C* c = [self bar];    // returns nil
  [foo:c];              // crash reported here
}
```

**Action**:
Adding a `nil` check either for `param` above or for `d`, or making sure that `foo:` will never
be called with `nil`.

Calling a `nil` block will also cause a crash.
We have a dedicated issue type for this case: [Nil Block Call](/docs/all-issue-types#nil_block_call).

Moreover, inserting `nil` into a collection will cause a crash as well. We
also have a dedicated issue type for this case:
[Nil Insertion Into Collection](/docs/all-issue-types#nil_insertion_into_collection).

## NULLPTR_DEREFERENCE_IN_NULLSAFE_CLASS

*Category: [Null pointer dereference](/docs/all-categories#null-pointer-dereference). Reported as "Null Dereference" by [pulse](/docs/checker-pulse).*

Infer reports null dereference bugs in Java, C, C++, and Objective-C
when it is possible that the null pointer is dereferenced, leading to
a crash.

### Null dereference in Java

Many of Infer's reports of potential Null Pointer Exceptions (NPE) come from code of the form

```java
  p = foo(); // foo() might return null
  stuff();
  p.goo();   // dereferencing p, potential NPE
```

If you see code of this form, then you have several options.

**If you are unsure whether or not `foo()` will return null**, you should
ideally either

1. Change the code to ensure that `foo()` can not return null, or

2. Add a check that `p` is not `null` before dereferencing `p`.

Sometimes, in case (2) it is not obvious what you should do when `p`
is `null`. One possibility is to throw an exception, failing early but
explicitly. This can be done using `checkNotNull` as in the following
code:

```java
// code idiom for failing early
import static com.google.common.base.Preconditions.checkNotNull;

  //... intervening code

  p = checkNotNull(foo()); // foo() might return null
  stuff();
  p.goo(); // p cannot be null here
```

The call `checkNotNull(foo())` will never return `null`: if `foo()`
returns `null` then it fails early by throwing a Null Pointer
Exception.

Facebook NOTE: **If you are absolutely sure that foo() will not be
null**, then if you land your diff this case will no longer be
reported after your diff makes it to trunk.

### Null dereference in C

Here is an example of an inter-procedural null dereference bug in C:

```c
struct Person {
  int age;
  int height;
  int weight;
};
int get_age(struct Person *who) {
  return who->age;
}
int null_pointer_interproc() {
  struct Person *joe = 0;
  return get_age(joe);
}
```

### Null dereference in Objective-C

In Objective-C, null dereferences are less common than in Java, but they still
happen and their cause can be hidden. In general, passing a message to nil does
not cause a crash and returns `nil`, but dereferencing a pointer directly does
cause a crash.

Example:

```objectivec
(int) foo:(C*) param {  // passing nil
  D* d = [param bar];   // nil message passing
  return d->fld;        // crash
}
(void) callFoo {
  C* c = [self bar];    // returns nil
  [foo:c];              // crash reported here
}
```

**Action**:
Adding a `nil` check either for `param` above or for `d`, or making sure that `foo:` will never
be called with `nil`.

Calling a `nil` block will also cause a crash.
We have a dedicated issue type for this case: [Nil Block Call](/docs/all-issue-types#nil_block_call).

Moreover, inserting `nil` into a collection will cause a crash as well. We
also have a dedicated issue type for this case:
[Nil Insertion Into Collection](/docs/all-issue-types#nil_insertion_into_collection).

## NULLPTR_DEREFERENCE_IN_NULLSAFE_CLASS_LATENT

*Category: [Null pointer dereference](/docs/all-categories#null-pointer-dereference). Reported as "Null Dereference" by [pulse](/docs/checker-pulse).*

A latent [NULLPTR_DEREFERENCE_IN_NULLSAFE_CLASS](#nullptr_dereference_in_nullsafe_class). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## NULLPTR_DEREFERENCE_LATENT

*Category: [Null pointer dereference](/docs/all-categories#null-pointer-dereference). Reported as "Null Dereference" by [pulse](/docs/checker-pulse).*

A latent [NULLPTR_DEREFERENCE](#nullptr_dereference). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## NULL_ARGUMENT

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Null Argument" by [pulse](/docs/checker-pulse).*

```objc
This issue type indicates `nil` being passed as argument where a non-nil value expected.

#import <Foundation/Foundation.h>

// Test (non-nil) returned values of NSString methods against `nil`
NSString* stringNotNil(NSString* str) {
  if (!str) {
        // ERROR: NSString:stringWithString: expects a non-nil value
	return [NSString stringWithString:nil];
  }
  return str;
}
```

## NULL_ARGUMENT_LATENT

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Null Argument Latent" by [pulse](/docs/checker-pulse).*

A latent [NULL_ARGUMENT](#null_argument). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## NULL_DEREFERENCE

*Category: [Null pointer dereference](/docs/all-categories#null-pointer-dereference). Reported as "Null Dereference" by [biabduction](/docs/checker-biabduction).*

See [NULLPTR_DEREFERENCE](#nullptr_dereference).
## OPTIONAL_EMPTY_ACCESS

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Optional Empty Access" by [pulse](/docs/checker-pulse).*

Optional Empty Access warnings are reported when we try to retrieve the value of a [`folly::Optional`](https://github.com/facebook/folly/blob/master/folly/Optional.h) when it is empty (i.e. `folly::none`).

In the following example we get a warning as `int_opt` might be `folly::none` and its value is being accessed:

```cpp
bool somef(int v);

folly::Optional<int> mightReturnNone(int v) {
   if (somef(v)) {
      return folly::Optional(v);
   }

   return folly::none;
}

int value_no_check() {
  folly::Optional<int> int_opt = mightReturnNone (4);
  return int_opt.value(); // Optional Empty Access warning
}
```

We do not get the warning anymore if we add a check whether `int_opt` is not empty:

```cpp
int value_check() {
  folly::Optional<int> int_opt = mightReturnNone (4);
  if (int_opt.has_value()) {
     return int_opt.value(); // OK
  }
  return -1;
}
```

In some cases we know that we have a non-empty value and there is no need to have a check. Consider the following example where Infer does not warn:

```cpp
bool somef(int v) {return v > 3;};

folly::Optional<int> mightReturnNone(int v) {
   if (somef(v)) {
      return folly::Optional(v);
   }

   return folly::none;
}

int value_no_check() {
  folly::Optional<int> int_opt = mightReturnNone (4); // cannot be folly::none
  return int_opt.value(); // OK
}
```

## OPTIONAL_EMPTY_ACCESS_LATENT

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Optional Empty Access Latent" by [pulse](/docs/checker-pulse).*

A latent [OPTIONAL_EMPTY_ACCESS](#optional_empty_access). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## PREMATURE_NIL_TERMINATION_ARGUMENT

*Reported as "Premature Nil Termination Argument" by [biabduction](/docs/checker-biabduction).*

This error type is reported in C and Objective-C. In many variadic methods,
`nil` is used to signify the end of the list of input objects. This is similar
to nil-termination of C strings. If one of the arguments that is not the last
argument to the method is `nil` as well, Infer reports an error because that may
lead to unexpected behavior.

An example of such variadic methods is
[arrayWithObjects](https://developer.apple.com/library/prerelease/ios/documentation/Cocoa/Reference/Foundation/Classes/NSArray_Class/index.html#//apple_ref/occ/clm/NSArray/arrayWithObjects)

```objectivec
  NSArray *foo = [NSArray arrayWithObjects: @"aaa", str, @"bbb", nil];
```

In this example, if `str` is `nil` then an array `@[@"aaa"]` of size 1 will be
created, and not an array `@[@"aaa", str, @"bbb"]` of size 3 as expected.

## PULSE_CANNOT_INSTANTIATE_ABSTRACT_CLASS

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Cannot Instantiate Abstract Class" by [pulse](/docs/checker-pulse).*

Instantiating an abstract class will lead to `Cannot instantiate abstract class` error.

```hack
abstract class AbstractClass1 {}

class ConcreteClass1 extends AbstractClass1 {}

public static function makeGeneric<T>(classname<T> $cls): void {
    new $cls();
}

<<__ConsistentConstruct>>
abstract class AbstractClass2 {

  public static function makeStatic(): void {
    new static();
  }
}

class ConcreteClass2 extends AbstractClass2 {}

public function badViaGeneric(): void {
    Main::makeGeneric(AbstractClass1::class); // ERROR!
}

public function goodViaGeneric(): void {
  Main::makeGeneric(ConcreteClass1::class);
}

public function badViaStatic(): void {
  AbstractClass2::makeStatic(); // ERROR!
}

public function goodViaStatic(): void {
  ConcreteClass2::makeStatic();
}
```

## PULSE_CONST_REFABLE

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Const Refable Parameter" by [pulse](/docs/checker-pulse).*

This issue is reported when a function parameter is a) passed by value and b) is not modified inside the function. Instead, parameter can be passed by const reference, i.e. converted to a `const&` so that no unnecessary copy is created at the callsite of the function.

For example,

```cpp
#include <vector>

int read_first(const std::vector<int>& vec) { return vec[0]; }

void const_refable(std::vector<int> vec) {
  int first = read_first(vec); // vec is never modified, so the parameter should have type const&
}
```

## PULSE_DICT_MISSING_KEY

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Dict Missing Key" by [pulse](/docs/checker-pulse).*

This issue is similar to [`PULSE_UNINITIALIZED_VALUE`](#pulse_uninitialized_value), but it is to warn
reading a missing key of dictionary in Hack.

For example, in the following code, the dictionary `$d` has no entry for `bye`, so reading
`$d['bye']` will throw the `OutOfBoundsException` exception, which is usually unexpected from
developers.  We can use a safer function `idx` instead when keys of a dictionary is unclear.

```hack
function simple_bad() : int {
  $d = dict['hi' => 42, 'hello' => 52];
  return $d['bye'];
}
```

## PULSE_DYNAMIC_TYPE_MISMATCH

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Dynamic Type Mismatch" by [pulse](/docs/checker-pulse).*

This error is reported in Hack. It fires when we detect an operation that is incompatible
with the dynamic type of its arguments.

For example, reading `$x['key']` when `$x` is a vector.


## PULSE_READONLY_SHARED_PTR_PARAM

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Read-only Shared Parameter" by [pulse](/docs/checker-pulse).*

This issue is reported when a shared pointer parameter is a) passed by value and b) is used only for reading, rather than lifetime extension. At the callsite, this might cause a potentially expensive unnecessary copy of the shared pointer, especially when many number of threads are sharing it. To avoid this, consider 1) passing the raw pointer instead and 2) use `std::shared_ptr::get` at callsites.

For example,

```cpp
void callee(std::shared_ptr<T> x) {
  // read_T(*x);
}

void caller() {
  callee(shared_ptr);
}
```

can be changed to

```cpp
void callee(T* p) {
  // read_T(*p);
}

void caller() {
  callee(shared_ptr.get());
}
```

## PULSE_REFERENCE_STABILITY

*Category: [Memory error](/docs/all-categories#memory-error). Reported as "Reference Stability" by [pulse](/docs/checker-pulse).*

The family of maps `folly::F14ValueMap`, `folly::F14VectorMap`, and by extension
`folly::F14FastMap` differs slightly from `std::unordered_map` as it does not
provide reference stability. When the map resizes such as when `reserve` is
called or new elements are added, all existing references become invalid and
should not be used.

`operator[]` is an interesting case as it can easily introduce unsafe code when
used twice in the same expression. Depending on what keys are present and which
order the compiler sequences sub-expressions, an insert via `operator[]` can
invalidate a reference obtained in the same expression before it's read from.
Typically, those cases can be improved by using other map functions such as
`at`, `find`, `emplace`, or `insert_or_assign` to increase code quality and
safety.

Examples:

```cpp
#include <folly/container/F14Map.h>

void use_reference_after_growth_bad(folly::F14FastMap<int, int>& map) {
  const auto& valueRef = map.at(1);
  map.emplace(13, 71);
  const auto valueCopy = valueRef;
}

void unsafe_expressions_bad(folly::F14FastMap<int, int>& map) {
  // Unsafe expressions in situations where one or both keys are not present.
  map[13] = map[71];
  const auto p = map[13] * map[71];
  const auto q = f(map[13], map[71]);
}
```

## PULSE_RESOURCE_LEAK

*Category: [Resource leak](/docs/all-categories#resource-leak). Reported as "Resource Leak" by [pulse](/docs/checker-pulse).*

See [RESOURCE_LEAK](#resource_leak)
## PULSE_TRANSITIVE_ACCESS

*Category: [Logic error](/docs/all-categories#logic-error). Reported as "Transitive Access" by [pulse](/docs/checker-pulse).*

This issue tracks spurious accesses that are reachable from specific entry functions.

Spurious accesses are specified as specific load/calls.

Entry functions are specified through their enclosing class that must extend a specific 
class and should not extend a list of specific classes.


## PULSE_UNAWAITED_AWAITABLE

*Category: [Resource leak](/docs/all-categories#resource-leak). Reported as "Unawaited Awaitable" by [pulse](/docs/checker-pulse).*

`Awaitable` values created by calls to asynchronous methods should eventually be `await`ed along all codepaths (even if their value is unused). Hence the following is *not* OK

```hack
class A {
  public static async genInt() : Awaitable<int>{
    // typically do something involving IO
  }

  public static async genBad() : Awaitable<void> {
    $_unused = self::genInt(); // ERROR: should have done $_unused = await self::genInt();
    return;
  }
}
```

Failure to `await` an `Awaitable` can lead to non-deterministic amount of the asynchronous call actually being executed, and can also indicate a logical confusion between `T` and `Awaitable<T>` that may not be caught by the type-checker.

## PULSE_UNINITIALIZED_CONST

*Category: [Runtime exception](/docs/all-categories#runtime-exception). Reported as "Uninitialized Const" by [pulse](/docs/checker-pulse).*

This issue is similar to [`PULSE_UNINITIALIZED_VALUE`](#pulse_uninitialized_value), but it is to detect the uninitialized abstract const value in Hack.

For example, in the following code, the `FIELD` can be read by the static method `get_field`.

* It is problematic invoking `static::FIELD`, since it may be resolved to a `A::FIELD` access, if called from `A::get_field()`. Because `FIELD` is abstract in `A`, it is never assigned a value and the vm will crash. Unfortunately, Hack's type system cannot catch this.
* In the `B` class, `FIELD` is initialized, thus invoking `B::get_field` is safe.

```hack
abstract class A {
  abstract const string FIELD;
  
  public static function get_field(): string {
    return static::FIELD;
  }
}

function call_get_field_bad(): string {
  return A::get_field();
}

class B extends A {
  const string FIELD = "defined";
}

function call_get_field_ok(): string {
  return B::get_field();
}
```

## PULSE_UNINITIALIZED_VALUE

*Category: [Memory error](/docs/all-categories#memory-error). Reported as "Uninitialized Value" by [pulse](/docs/checker-pulse).*

The code uses a variable that has not been initialized, leading to unpredictable or unintended results.

Using uninitialized values can lead to undefined behaviors possibly resulting in crashes, security failures and invalid results.

This can easily be fixed by assigning all variables to an initial value when declaring them.

This, for example, in C:

```c
struct coordinates {
  int x;
  int y;
};

void foo() {
  struct coordinates c;
  c.x = 42;
  c.y++; // uninitialized value c.y!

  int z;
  if (z == 0) { // uninitialized value z!
    // something
  }
}
```

## PULSE_UNNECESSARY_COPY

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Unnecessary Copy" by [pulse](/docs/checker-pulse).*

This is reported when Infer detects an unnecessary copy of an object via copy constructor where neither the source nor the copied variable are modified before the variable goes out of scope. Rather than the copy, a reference to the source object could be used to save memory.

For example,

```cpp
struct A {
  int a;
};

int unnecessary_copy(A& x){
  auto y = x; // calls copy constructor
  return y.a; // y is not modified after copy, hence we could avoid the copy by adding & after auto as below
}

int use_reference_instead(A& x){
  auto& y = x; // copy the ref only
  return y.a;
}
```
## PULSE_UNNECESSARY_COPY_ASSIGNMENT

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Unnecessary Copy Assignment" by [pulse](/docs/checker-pulse).*

See [PULSE_UNNECESSARY_COPY](#pulse_unnecessary_copy).
## PULSE_UNNECESSARY_COPY_ASSIGNMENT_CONST

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Unnecessary Copy Assignment from Const" by [pulse](/docs/checker-pulse).*

See [PULSE_UNNECESSARY_COPY](#pulse_unnecessary_copy).
## PULSE_UNNECESSARY_COPY_ASSIGNMENT_MOVABLE

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Unnecessary Copy Assignment Movable" by [pulse](/docs/checker-pulse).*

See [PULSE_UNNECESSARY_COPY_MOVABLE](#pulse_unnecessary_copy_movable).
## PULSE_UNNECESSARY_COPY_INTERMEDIATE

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Unnecessary Copy Intermediate" by [pulse](/docs/checker-pulse).*

This is reported when Infer detects an unnecessary temporary copy of an intermediate object where copy is created to be passed down to a function unnecessarily. Instead, the intermediate object should either be moved into the callee or the type of the callee's parameter should be made `const &`.

A prime example of this occurs when we call a function with a call-by-value parameter as follows:

```cpp
void callee(ExpensiveObject obj) {
  // ....
}

void caller() {
  callee(myExpensiveObj); // a copy of myExpensiveObj is created
  // the copy is destroyed right after the call  
}
```

In this case, when we call `callee`, under the hood, a copy of the argument `myExpensiveObj` is created to be passed to the function call. However, the copy might be unnecessary if

 -   `callee` doesn’t modify its parameter → then we can change its type to `const ExpensiveObject&`, getting rid of the copy at caller
 -   even if `callee` might modify the object, if the argument `myExpensiveObj` is never used later on, we can get rid of the copy by moving it instead: `callee(std::move(myExpensiveObj))`.


The analysis is careful about suggesting moves blindly though: if the argument `myExpensiveObj` is of type `const & ExpensiveObject` then we also recommend that for move to work, const-reference needs to be removed.


PS: We check for other conditions on the argument here: e.g. it should be local to the procedure, as moving a non-local member might cause other memory correctness issues like use-after-move later on.
## PULSE_UNNECESSARY_COPY_INTERMEDIATE_CONST

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Unnecessary Copy Intermediate from Const" by [pulse](/docs/checker-pulse).*

See [PULSE_UNNECESSARY_COPY](#pulse_unnecessary_copy).
## PULSE_UNNECESSARY_COPY_MOVABLE

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Unnecessary Copy Movable" by [pulse](/docs/checker-pulse).*

This is reported when Infer detects an unnecessary copy into a field where
- the source is an rvalue-reference
- the source is not modified before it goes out of scope or is destroyed.

Note that the copy can be modified since it has the ownership of the object.

Fix: Rather than the copying into the field, the source should be moved into it.

For example,

```cpp
struct A {
  std::vector<int> vec;
};

class Test {
  A mem_a;

  void unnecessary_copy(A&& src) {
   mem_a = src;
   // fix is to move as follows
   // mem_a = std::move(src);
  }

};

```
## PULSE_UNNECESSARY_COPY_OPTIONAL

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Unnecessary Copy to Optional" by [pulse](/docs/checker-pulse).*

This is reported when Infer detects an unnecessary copy of an object via `optional` value
construction where the source is not modified before it goes out of scope.  To avoid the copy, we
can move the source object or change the callee's type.

For example,

```cpp
void get_optional_value(std::optional<A> x) {}

void pass_non_optional_value(A x) {
  get_optional_value(x);
  // fix is to move as follows
  // get_optional_value(std::move(x));
}
```

## PULSE_UNNECESSARY_COPY_OPTIONAL_CONST

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Unnecessary Copy to Optional from Const" by [pulse](/docs/checker-pulse).*

See [PULSE_UNNECESSARY_COPY_OPTIONAL](#pulse_unnecessary_copy_optional).
## PULSE_UNNECESSARY_COPY_RETURN

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Unnecessary Copy Return" by [pulse](/docs/checker-pulse).*

This is similar to [PULSE_UNNECESSARY_COPY](#pulse_unnecessary_copy), but reported when a callee returns a copied value and it is not modified in its caller.  We may be able to return const-ref typed value or try `std::move` to avoid the copy.

For example,

```cpp
class MyClass {
  T v;
 public:
  T get() {
    return v; // v is copied here, which is avoidable.
  }
};

void caller(MyClass obj) {
  T x = obj.get();
  std::cout << x; // x is not modified.
}
```

## PURE_FUNCTION

*Reported as "Pure Function" by [purity](/docs/checker-purity).*

This issue type indicates pure functions. For instance, below functions would be marked as pure:

```java
int local_write_pure(int x, int y) {
  int k = x + y;
  k++;
  return k;
}

// no change to outside state, the local allocation is ok.
int local_alloc_pure(ArrayList<Integer> list) {
  ArrayList<Integer> list_new = new ArrayList<Integer>();
  for (Integer el : list) {
    list_new.add(el);
  }
  return list_new.size();
}
```

However, the following ones would not be pure:

```java
void swap_impure(int[] array, int i, int j) {
  int tmp = array[i];
  array[i] = array[j]; // modifying the input array
  array[j] = tmp;
}

int a = 0;
void set_impure(int x, int y) {
  a = x + y; //modifying a global variable
}
```

## QUANDARY_TAINT_ERROR

*Reported as "Taint Error" by [quandary](/docs/checker-quandary).*

Generic taint error when nothing else fits.
## REGEX_OP_ON_UI_THREAD

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Regex Op On Ui Thread" by [starvation](/docs/checker-starvation).*

A potentially costly operation on a regular expression occurs on the UI thread.
## RESOURCE_LEAK

*Category: [Resource leak](/docs/all-categories#resource-leak). Reported as "Resource Leak" by [biabduction](/docs/checker-biabduction).*

Infer reports resource leaks in C, Objective-C and Java. In general, resources
are entities such as files, sockets, connections, etc, that need to be closed
after being used.

### Resource leak in C

This is an example of a resource leak in C code:

```c
-(void) resource_leak_bug {
    FILE *fp;
    fp=fopen("c:\\test.txt", "r"); // file opened and not closed.
}
```

### Resource leak in Java

For the remaining of this section, we will consider examples of resource leaks
in Java code.

TIP: A common source of bugs is <b>exceptions skipping past close()
statements</b>. That is the first thing to look for if INFER reports a potential
resource leak.

### Basics and Standard Idiom

Some objects in Java, the <i>resources</i>, are supposed to be closed when you
stop using them, and failure to close is a <i>resource leak</i>. Resources
include input streams, output streams, readers, writers, sockets, http
connections, cursors, and json parsers.

The standard idiom is

```java
  // Standard idiom
  Allocate resource
  try {
    do some stuff
  } finally {
    close resource
  }
```

or more for example,

```java
  //  Standard Idiom
  public static void foo () throws IOException{
    FileOutputStream fos = new FileOutputStream(new File("whatever.txt"));
    try {
      fos.write(7);
    } finally {
      fos.close();
    }
  }
```

and you should use the standard idiom for the most part, when you don't want to
return the resource to the surrounding context.

Sometimes people just leave out close(), and that is a bug, but more typically
exceptional paths are the root of the problem, as in

```java
  // leak because of exception
  public static void foo () throws IOException {
    FileOutputStream fos = new FileOutputStream(new File("whatever.txt"));
    fos.write(7);   //DOH! What if exception?
    fos.close();
  }
```

where an exception in fos.write will cause execution to skip past the close()
statement.

#### Multiple Resources Bugs

We can deal with multiple resources correctly and simply just by nesting the
standard idiom.

```java
  // Two Resources nested
  public static void foo() throws IOException {
    FileInputStream fis = new FileInputStream(new File("whatever.txt"));
    try {
      FileOutputStream fos = new FileOutputStream(new File("everwhat.txt"));
      try {
        fos.write(fis.read());
      } finally {
        fos.close();
      }
    } finally {
      fis.close();
    }
  }
```

Bugs often occur when using multiple resources in other ways because of
exceptions in close() methods. For example,

```java
  // Classic Two Resources Bug
  public static void foo() throws IOException {
    FileInputStream fis = null;
    FileOutputStream fos = null;
    try {
      fis = new FileInputStream(new File("whatever.txt"));
      fos = new FileOutputStream(new File("everwhat.txt"));
      fos.write(fis.read());
    } finally {
      if (fis!=null)  fis.close();
      if (fos!=null) fos.close();
    }
  }
```

Here, if there is an exception in the call to fis.close() execution will skip
past fos.close(); a leak.

Another way, besides the standard idiom, to deal with this problem is to swallow
exceptions.

```java
  // Two Resources Fix 1
  public static void foo() throws IOException {
    FileInputStream fis = null;
    FileOutputStream fos = null;
    try {
      fis = new FileInputStream(new File("whatever.txt"));
      fos = new FileOutputStream(new File("everwhat.txt"));
      fos.write(fis.read());
    } finally {
      try {
        if (fis!=null) fis.close();
      } catch (Exception e) {};  // Exception swallowing
      if (fos!=null) fos.close();
    }
  }
```

You can also swallow the exception on the output stream. Some people prefer not
to swallow output stream exceptions, and also flush before closing.
http://code.google.com/p/guava-libraries/issues/detail?id=1118

Notice that the nested standard idiom does not need the checks for null, which
are in there in this case to protect against the case when one of the
allocations throws an exception, in which case one would get a
NullPointerException.

### Nested_Allocations

When a resource allocation is included as an argument to a constructor, if the
constructor fails it can leave an unreachable resource that no one can close.

For example gzipOutputStream = new GZIPOutputStream(new FileOutputStream(out));
is bad in case the outer constructor, GZIPOutputStream, throws an exception. In
that case, no one will have a hold of the FileOutputStream and so no one will be
able to close it.

In such a case you need to move the allocation the FileOutputStream out of the
nested position and name it, so you are able to close if anything goes wrong
during execution of the GZIPOutputStream constructor.

Here are resources that can throw exceptions i their constructor(s).

- ObjectInputStream , ObjectOutputStream, PipedInputStream, PipedOutputStream,
  PipedReader, PipedWriter, JarInputStream, JarOutputStream, GZIPInputStream,
  GZIPOutputStream , ZipFile all throw IOException
- PrintStream throws UnsupportedEncodingException

The constructors for FileInputStream, FileOutputStream and RandomAccessFile
throw FileNotFoundException, but these cases are not problematic in the sense
that their arguments are not resources and so they do not cause the nested
resource leak.

### Allocation of JSonParser and Cursor resources

Some resources are created inside libraries instead of by "new".

Cursor is an interface, the actual resources are something like SQLiteCursor.
So, every time you call a function that returns a Cursor object, there is an
allocation.

For instance, in the functions from SQLiteDatabase query(…) and rawQuery(…)
allocate a cursor resource. For SQLiteQueryBuilder, ContentProviderClient,
ContentResolver. MediaStore and DownloadManager it is only query(…) Cursor
objects cursor created by these functions need to be closed (i.e.,
cursor.close()).

Similarly, JsonParser is an abstract class, and create a resource in functions
from the class JsonFactory createParser(byte[] data) createParser(byte[] data,
int offset, int len) createParser(String content) createParser(URL url)
createParser(File f) JsonParser objects js created by these functions need to be
closed (jp.close()). On the other hand . JasonParsers gotten from
createParser(InputStream in) and createParser(Reader r) give you JsonParsers
that don’t need to be closed. This is because they receive the resource from
somewhere that will maintain the responsibility to close it.

### Escaping resources and exceptions

Sometimes you want to return a resource to the outside, in which case you should
not close it, but you still need to be careful of exceptions in case control
skips past the return leaving no one to close. Here is a simple example of a
positive use of escaping resources.

```java
  // An escaping resource, shouldn't close
  public BugReportAttachment createAttachment(File reportDirectory, String fileName)
      throws FileNotFoundException {
    File file = new File(reportDirectory, fileName);
    OutputStream stream = new FileOutputStream(file);
    return new BugReportAttachment(Uri.fromFile(file), stream);
  }
```

In this case it is intended that an object that wraps `stream` is passed to the
caller of `createAttachment`. You should certainly not close stream here,
because it is being passed to the outside.

But for escaping resources like this you still need to be careful of exceptions.
For example, in

```java
  // An escaping resource, and a leak
  public BugReportAttachment createAttachment(File reportDirectory, String fileName)
      throws FileNotFoundException {
    File file = new File(reportDirectory, fileName);
    OutputStream stream = new FileOutputStream(file);
    stream.write(7);
    return new BugReportAttachment(Uri.fromFile(file), stream);
  }
```

if stream.write(7) throws an exception, then no one will have a hold of stream,
and no one will be able to close it; a leak.

### Java 7's try-with-resources

**(For use with Java 7 only)**

Clearly, accounting for the ramifications of all the exceptional cases is
complicated, and there is a better way in Java 7.

```java
  // Two Resources Fix 2; via try-with-resources
  public static void foo() throws IOException {
    try (
      FileInputStream fis = new FileInputStream(new File("whatever.txt"));
      FileOutputStream fos = new FileOutputStream(new File("everwhat.txt"))
    ) {
      fos.write(fis.read());
    }
  }
```

All the complicated exceptional cases above are (apparently) covered by this
construct, and the result is much simpler.

So, if you are trying to fix a potential leak in code with multiples resources
you can go ahead and try to understand whether the potential leak is real. Or,
if the code is complex and it is hard to figure out, it would be perfectly
legitimate to simply convert the code over to try-with-resources if you have
access to Java 7, so as to save yourself some brain-cycles. You will also end up
with cleaner code.

If try-with-resources is so great you should <i>always</i> use it. But you
shouldn't… Try-with-resources gives resources static scoping, and works via a
stack discipline. Sometimes, you want a resource to persist beyond scope, as in
the escaping example above. In an escaping example maybe you could refactor lots
of code so that try-with-resources applies, and maybe you cannot in a sensible
way. This just illustrates that, though you might hear people say that
try-with-resources "solves" the resource problem, it does not. It is very
useful, but you cannot use it blindly when you see a resource-allocation site.

## RETAIN_CYCLE

*Category: [Resource leak](/docs/all-categories#resource-leak). Reported as "Retain Cycle" by [pulse](/docs/checker-pulse).*

A retain cycle is a situation when object A retains object B, and object B
retains object A at the same time. Here is an example:

```objectivec
@class Child;
@interface Parent : NSObject {
    Child *child; // Instance variables are implicitly __strong
}
@end
@interface Child : NSObject {
    Parent *parent;
}
@end
```

You can fix a retain cycle in ARC by using \_\_weak variables or weak properties
for your "back links", i.e. links to direct or indirect parents in an object
hierarchy:

```objectivec
@class Child;
@interface Parent : NSObject {
    Child *child;
}
@end
@interface Child : NSObject {
    __weak Parent *parent;
}
@end
```

## RETAIN_CYCLE_NO_WEAK_INFO

*Category: [Resource leak](/docs/all-categories#resource-leak). Reported as "Retain Cycle No Weak Info" by [pulse](/docs/checker-pulse).*

A retain cycle is a situation when object A retains object B, and object B
retains object A at the same time. Here is an example:

```objectivec
@class Child;
@interface Parent : NSObject {
    Child *child; // Instance variables are implicitly __strong
}
@end
@interface Child : NSObject {
    Parent *parent;
}
@end
```

You can fix a retain cycle in ARC by using \_\_weak variables or weak properties
for your "back links", i.e. links to direct or indirect parents in an object
hierarchy:

```objectivec
@class Child;
@interface Parent : NSObject {
    Child *child;
}
@end
@interface Child : NSObject {
    __weak Parent *parent;
}
@end
```

## SCOPE_LEAKAGE

*Category: [Sensitive data flow](/docs/all-categories#sensitive-data-flow). Reported as "Scope Leakage" by [scope-leakage](/docs/checker-scope-leakage).*

This issue type indicates that a class with scope annotation A stores a field
with whose (dynamic) type (or one of its super types) is annotated with scope
B such that a scope nesting restriction is violated. By "stores", we mean
either directly or transitively.

A configuration is used to list the set of scopes and the must-not-hold relation.

In the following Java example, the set of scopes is Outer and Inner, and the must-not-hold
relation is simply \{(Outer, Inner)\}:
```java
@ScopeType(value = Outer.class)
class ClassOfOuterScope {
  final ClassOfInner c = new ClassOfInner(); // <-- warn here that ClassOfInner would leak.
}

@ScopeType(value = Inner.class)
class ClassOfInner {}
```

Here is a more detailed description of the analysis.

This analysis operates over Java bytecode. It assumes that types (classes, interfaces, enums,
etc.) may be annotated with so-called scope annotations. The analysis is parameterized by a set
of scopes and a "must-not-hold" relation over pairs of scopes, which it reads from a
configuration file.

The analysis aims to detect violations of the following property: if there exist a path of
fields from object OA to object OB and the type of OA (or one of its super-types) is annotated
with scope SA and the type of OB (or one of its super-types) is annotated with scope SB then
must-not-hold(SA, SB) must be false. Intuitively, the given objects have different scopes that
should not be nested, for example, different intended lifetimes, and a forbidden path from OA to
OB results in OB "leaking" out of the scope SA.

The implementation reads a configuration to determine a list of (scope) "generators" for each
type of scope and a scope class for each type of scope. A generator for a scope type SA is given
by the name of a class and a list of methods where it is understood that any of the methods
listed for the given class returns an object that is known to have scope SA. (This can be seen
as a form of lightweight modeling.) A scope class is the name of the class that represents a
given scope.

## SENSITIVE_DATA_FLOW

*Category: [Sensitive data flow](/docs/all-categories#sensitive-data-flow). Reported as "Sensitive Data Flow" by [pulse](/docs/checker-pulse).*

A flow of sensitive data was detected from a source.
## SHELL_INJECTION

*Reported as "Shell Injection" by [quandary](/docs/checker-quandary).*

Environment variable or file data flowing to shell.
## SHELL_INJECTION_RISK

*Reported as "Shell Injection Risk" by [quandary](/docs/checker-quandary).*

Code injection if the caller of the endpoint doesn't sanitize on its end.
## SQL_INJECTION

*Reported as "Sql Injection" by [quandary](/docs/checker-quandary).*

Untrusted and unescaped data flows to SQL.
## SQL_INJECTION_RISK

*Reported as "Sql Injection Risk" by [quandary](/docs/checker-quandary).*

Untrusted and unescaped data flows to SQL.
## STACK_VARIABLE_ADDRESS_ESCAPE

*Category: [Memory error](/docs/all-categories#memory-error). Reported as "Stack Variable Address Escape" by [pulse](/docs/checker-pulse).*

Reported when an address pointing into the stack of the current
function will escape to its calling context. Such addresses will
become invalid by the time the function actually returns so are
potentially dangerous.

For example, directly returning a pointer to a local variable:

```C
int* foo() {
   int x = 42;
   return &x; // <-- warn here that "&x" will escape
}
```

## STARVATION

*Reported as "UI Thread Starvation" by [starvation](/docs/checker-starvation).*

This error is reported in Java, and specifically on Android. These reports are
triggered when a method that runs on the UI thread may block, thus potentially
leading to an Application Not Responding error.

Infer considers a method as running on the UI thread whenever:

- The method, one of its overrides, its class, or an ancestral class, is
  annotated with `@UiThread`.
- The method, or one of its overrides is annotated with `@OnEvent`, `@OnClick`,
  etc.
- The method or its callees call a `Litho.ThreadUtils` method such as
  `assertMainThread`.

The issue is reported when a method deemed to run on the UI thread

- Makes a method call which may block.
- Takes a lock, and another thread takes the same lock, and before releasing it,
  makes a call that may block.

Calls that may block are considered:

- Certain I/O calls.
- Two way `Binder.transact` calls.
- Certain OS calls.
- `Future` or `AsyncTask` calls to `get` without timeouts, or with too large
  timeouts.

To suppress starvation reports in a method `m()` use the
`@SuppressLint("STARVATION")` annotation, as follows:

```java
  import android.annotation.SuppressLint;

  @SuppressLint("STARVATION")
  public void m() {
  ...
  }
```

To signal to Infer that a method does not perform any blocking calls, despite
appearences, you can use the `@NonBlocking` annotation:

```java
  import com.facebook.infer.annotation.NonBlocking;

  @NonBlocking
  public void m() {
  ...
  }
```

This instructs Infer to filter out any potentially blocking calls in `m()`
(also, transitively), and thus any other method can expect no starvation reports
due to a call to `m()`. You will need to set up your class path appropriately to
include the JAR files in `infer/annotations` for this annotation to work.

## STATIC_INITIALIZATION_ORDER_FIASCO

*Reported as "Static Initialization Order Fiasco" by [siof](/docs/checker-siof).*

This error is reported in C++. It fires when the initialization of a static
variable `A`, accesses a static variable `B` from another translation unit
(usually another `.cpp` file). There are no guarantees whether `B` has been
already initialized or not at that point.

For more technical definition and techniques to avoid/remediate, see the
[FAQ](https://isocpp.org/wiki/faq/ctors#static-init-order).

## STRICT_MODE_VIOLATION

*Category: [Perf regression](/docs/all-categories#perf-regression). Reported as "Strict Mode Violation" by [starvation](/docs/checker-starvation).*

Android has a feature called
[strict mode](https://developer.android.com/reference/android/os/StrictMode),
which if enabled, will flag the occasions where the main thread makes a call
that results in disk I/O, waiting on a network socket, etc. The analysis
catching starvation errors and deadlocks (the `--starvation` analysis) has the
ability to statically detect such violations.

To suppress this warning, it's enough to annotate the offending method with
`@SuppressLint("STRICT_MODE_VIOLATION")`.

## STRONG_SELF_NOT_CHECKED

*Category: [Memory error](/docs/all-categories#memory-error). Reported as "StrongSelf Not Checked" by [self-in-block](/docs/checker-self-in-block).*

This checks reports a potential issue when a block captures `weakSelf` (a weak pointer to `self`),
then one assigns this pointer to a local variable `strongSelf` inside the block and uses this variable
without checking first whether it is `nil`. The problem here is that the weak pointer could be `nil` at
the time when the block is executed. So, the correct usage is to first check whether `strongSelf` is a valid
pointer, and then use it.

Example:

```objectivec
__weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    int y = strongSelf->x;
    ...
```

**Action:**
Add a check for `nil`:

```objectivec
__weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf) {
      int y = strongSelf->x;
      ...
    }
```

*Limitations:* To keep this check simple and intra-procedural, we rely on names to find `weakSelf`:
we assume that any captured weak pointer whose name contains "self" is a weak reference to `self`.
In contrast, `strongSelf` is a local variable to the block, so the check supports any name given to
a local strong pointer that has been assigned `weakSelf`.

## TAINT_ERROR

*Category: [Sensitive data flow](/docs/all-categories#sensitive-data-flow). Reported as "Taint Error" by [pulse](/docs/checker-pulse).*

A taint flow was detected from a source to a sink
## THREAD_SAFETY_VIOLATION

*Category: [Concurrency](/docs/all-categories#concurrency). Reported as "Thread Safety Violation" by [racerd](/docs/checker-racerd).*

This warning indicates a potential data race in Java. The analyser is called
RacerD and this section gives brief but a mostly complete description of its
features. See the [RacerD page](/docs/checker-racerd) for more in-depth information and
examples.

### Thread-safety: What is a data race

Here a data race is a pair of accesses to the same member field such that:

- at least one is a write, and,
- at least one occurs without any lock synchronization, and,
- the two accesses occur on threads (if known) which can run in parallel.

### Thread-safety: Potential fixes

- Synchronizing the accesses (using the `synchronized` keyword, thread-exclusion
  such as atomic objects, `volatile` etc).
- Making an offending method private -- this will exclude it from being checked
  at the top level, though it will be checked if called by a public method which
  may itself, e.g., hold a lock when calling it.
- Putting the two accesses on the same thread, e.g., by using `@MainThread` or
  `@ThreadConfined`.

### Thread-safety: Conditions checked before reporting

The class and method are not marked `@ThreadSafe(enableChecks = false)`, and,

- The method is declared `synchronized`, or employs (non-transitively) locking,
  or,
- The class is not marked `@NotThreadSafe`, and,
  - The class/method is marked `@ThreadSafe,` or one of the configured synonyms
    in `.inferconfig`, or,
  - A parent class, or an override method are marked with the above annotations.

NB currently RacerD **does not take into account `@GuardedBy`**.

### Thread-safety: Thread annotations recognized by RacerD

These class and method annotations imply the method is on the main thread:
`@MainThread`, `@UiThread`

These method annotations imply the method is on the main thread: `@OnBind`,
`@OnEvent`, `@OnMount`, `@OnUnbind`, `@OnUnmount`

Both classes of annotations work through the inheritance tree (i.e. if a parent
class or method is marked with one of these annotations, so is the child class /
method override).

In addition to these, RacerD recognizes many lifecycle methods as necessarily
running on the main thread, eg `Fragment.onCreate` etc.

Finally, the thread status of being on the main thread propagates backwards
through the call graph (ie if `foo` calls `bar` and `bar` is marked `@UiThtread`
then `foo` is automatically considered on the main thread too). Calling
`assertMainThread`, `assertOnUiThread`, `checkOnMainThread` has the same effect.

NB RacerD currently **does not recognize `@WorkerThread`, `@BinderThread` or
`@AnyThread`**.

### Thread-safety: Other annotations and what they do

These annotations can be found at `com.facebook.infer.annotation.*`.

- `@Functional` This is a method annotation indicating the method always returns
  the same value. When a method `foo` is annotated `@Functional`, RacerD will
  ignore any writes of the return value of `foo`. For example, in
  `this.x = foo()`, the write to `this.x` is ignored. The reasoning is that if
  the method returns the same value whenever it's called, any data race on
  `this.x` is benign, if that is the only write.

- `@ThreadConfined` This is a class/method/field annotation which takes a single
  parameter which can be `UI`, `ANY` or a user chosen string. It indicates to
  RacerD a thread identifier for the class/method/field. Thus,
  `@ThreadConfined(UI)` is equivalent to `@UiThread`, and `@ThreadConfined(ANY)`
  is equivalent to not having the annotation at all, for classes and methods.
  When this annotation is applied to a field it instructs Infer to assume
  (without checking) that all accesses to that field are made on the same thread
  (and can, therefore, not race by definition). The intention is that RacerD
  uses that to detect exclusion between accesses occurring on the same thread.
  However, only the UI thread is supported at this time, and any user provided
  value is considered equal to `UI`.

- `@VisibleForTesting` A method annotation making Infer consider the method as
  effectively `private`. This means it will not be checked for races against
  other non-private methods of the class, but only if called by one.

- `@ReturnsOwnership` A method annotation indicating that the method returns a
  freshly owned object. Accesses to the returned value will not be considered
  for data races, as the object is in-effect unique and not accessible yet from
  other threads. The main utility of this annotation is in interfaces, where
  Infer cannot look up the implementation and decide for itself.

## TOPL_ERROR

*Category: [Sensitive data flow](/docs/all-categories#sensitive-data-flow). Reported as "Topl Error" by [topl](/docs/checker-topl).*

A violation of a Topl property (user-specified).
There is an execution path in the code that drives a Topl property from a start state to an error state.

This indicates that the code has a user-defined undesired behavior.

See [Topl](/docs/checker-topl#what-is-it) for an example

## TOPL_ERROR_LATENT

*Category: [Sensitive data flow](/docs/all-categories#sensitive-data-flow). Reported as "Topl Error Latent" by [topl](/docs/checker-topl).*

A latent [TOPL_ERROR](#topl_error). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## UNTRUSTED_BUFFER_ACCESS

*Reported as "Untrusted Buffer Access" by [quandary](/docs/checker-quandary).*

Untrusted data of any kind flowing to buffer.
## UNTRUSTED_DESERIALIZATION

*Reported as "Untrusted Deserialization" by [quandary](/docs/checker-quandary).*

User-controlled deserialization.
## UNTRUSTED_DESERIALIZATION_RISK

*Reported as "Untrusted Deserialization Risk" by [quandary](/docs/checker-quandary).*

User-controlled deserialization
## UNTRUSTED_ENVIRONMENT_CHANGE_RISK

*Reported as "Untrusted Environment Change Risk" by [quandary](/docs/checker-quandary).*

User-controlled environment mutation.
## UNTRUSTED_FILE

*Reported as "Untrusted File" by [quandary](/docs/checker-quandary).*

User-controlled file creation; may be vulnerable to path traversal and more.
## UNTRUSTED_FILE_RISK

*Reported as "Untrusted File Risk" by [quandary](/docs/checker-quandary).*

User-controlled file creation; may be vulnerable to path traversal and more.
## UNTRUSTED_HEAP_ALLOCATION

*Reported as "Untrusted Heap Allocation" by [quandary](/docs/checker-quandary).*

Untrusted data of any kind flowing to heap allocation. this can cause crashes or DOS.
## UNTRUSTED_INTENT_CREATION

*Reported as "Untrusted Intent Creation" by [quandary](/docs/checker-quandary).*

Creating an Intent from user-controlled data.
## UNTRUSTED_URL_RISK

*Reported as "Untrusted Url Risk" by [quandary](/docs/checker-quandary).*

Untrusted flag, environment variable, or file data flowing to URL.
## UNTRUSTED_VARIABLE_LENGTH_ARRAY

*Reported as "Untrusted Variable Length Array" by [quandary](/docs/checker-quandary).*

Untrusted data of any kind flowing to stack buffer allocation. Trying to allocate a stack buffer that's too large will cause a stack overflow.
## USER_CONTROLLED_SQL_RISK

*Reported as "User Controlled Sql Risk" by [quandary](/docs/checker-quandary).*

Untrusted data flows to SQL (no injection risk).
## USE_AFTER_DELETE

*Category: [Memory error](/docs/all-categories#memory-error). Reported as "Use After Delete" by [pulse](/docs/checker-pulse).*

An address that was invalidated by a call to `delete` in C++ is dereferenced.

## USE_AFTER_DELETE_LATENT

*Category: [Memory error](/docs/all-categories#memory-error). Reported as "Use After Delete Latent" by [pulse](/docs/checker-pulse).*

A latent [USE_AFTER_DELETE](#use_after_delete). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## USE_AFTER_FREE

*Category: [Memory error](/docs/all-categories#memory-error). Reported as "Use After Free" by [pulse](/docs/checker-pulse).*

An address that was invalidated by a call to `free` in C is dereferenced.

## USE_AFTER_FREE_LATENT

*Category: [Memory error](/docs/all-categories#memory-error). Reported as "Use After Free Latent" by [pulse](/docs/checker-pulse).*

A latent [USE_AFTER_FREE](#use_after_free). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## USE_AFTER_LIFETIME

*Category: [Memory error](/docs/all-categories#memory-error). Reported as "Use After Lifetime" by [pulse](/docs/checker-pulse).*

The lifetime of an object has ended but that object is being
accessed. For example, the address of a variable holding a C++ object
is accessed after the variable has gone out of scope:

```cpp
void foo() {
     X* p;
     { // new scope
       X x = X();
       p = &x;
     } // x has gone out of scope
     p->method(); // ERROR: you should not access *p after x has gone out of scope
}
```

## USE_AFTER_LIFETIME_LATENT

*Category: [Memory error](/docs/all-categories#memory-error). Reported as "Use After Lifetime Latent" by [pulse](/docs/checker-pulse).*

A latent [USE_AFTER_LIFETIME](#use_after_lifetime). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## VECTOR_INVALIDATION

*Category: [Memory error](/docs/all-categories#memory-error). Reported as "Vector Invalidation" by [pulse](/docs/checker-pulse).*

An address pointing into a C++ `std::vector` might have become
invalid. This can happen when an address is taken into a vector, then
the vector is mutated in a way that might invalidate the address, for
example by adding elements to the vector, which might trigger a
re-allocation of the entire vector contents (thereby invalidating the
pointers into the previous location of the contents).

For example:

```cpp
void deref_vector_element_after_push_back_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  int* y = elt;
  vec.push_back(42); // if the array backing the vector was full already, this
                     // will re-allocate it and copy the previous contents
                     // into the new array, then delete the previous array
  std::cout << *y << "\n"; // bad: y might be invalid
}
```

## VECTOR_INVALIDATION_LATENT

*Category: [Memory error](/docs/all-categories#memory-error). Reported as "Vector Invalidation Latent" by [pulse](/docs/checker-pulse).*

A latent [VECTOR_INVALIDATION](#vector_invalidation). See the [documentation on Pulse latent issues](/docs/checker-pulse#latent-issues).
## WEAK_SELF_IN_NO_ESCAPE_BLOCK

*Reported as "Weak Self In No Escape Block" by [self-in-block](/docs/checker-self-in-block).*

This check reports when `weakSelf` (a weak pointer to `self`) is used in
a block, and this block is passed to a "no escaping" method. This means that
the block passed to that method won't be leaving the current scope, this is
marked with the annotation `NS_NOESCAPE`.

The issue here is that, because the block is "no escaping", there is no need to use
`weakSelf` and `strongSelf` but we can just use `self`. This has the advantage of
not needing to deal with the added complexity of weak pointers, and it simplifies the
code.

Example:

```objectivec
  __weak __typeof(self) weakSelf = self;
  [self foo:^() { //foo's first parameter is annotates with `NS_NOESCAPE`
      [weakSelf bar];
  }];
```

**Action**:

Replace `weakSelf` with `self`:

```objectivec
  [self foo:^() {
      [self bar];
  }];
```

*Limitations:* To keep this check simple and intra-procedural, we rely on names to find `weakSelf`:
we assume that any captured weak pointer whose name contains "self" is a weak reference to `self`.

