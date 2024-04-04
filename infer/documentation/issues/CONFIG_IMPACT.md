Infer reports this issue when an *expensive* function is called without a *config check*.  The
*config* is usually a boolean value that enables experimental new features and it is defined per
application/codebase, e.g. gatekeepers.  To determine whether a function is expensive or not, the
checker relies on modeled functions that are assumed to be expensive, e.g. string operations,
regular expression match, or DB accesses.

Similar to [Cost analysis](/docs/next/checker-cost), this issue type is reported only in
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
