Infer reports this issue when an expensive function is called without a config check. Configs are usually functions that return a boolean and are defined per application/codebase (e.g. gatekeepers). This issue type is only reported in differential mode: i.e when we are
comparing the analysis results of two runs of infer on a file.


To determine whether a function is expensive or not, we rely on Infer's cost analysis and a set of modeled functions that are assumed to be expensive (e.g. string operations).

For instance, if we have the following code (v1)
```java
foo();
if (config_check){
   bar();
}
```
which is then modified to the version (v2)
```java
foo();
if (config_check){
   bar();
}
goo(); // added
```
the analysis would warn the developer that `goo()` is a newly added (and assumed to be expensive) but ungated function that might cause a new behavior. However, if we were to add `goo()` right after `bar()`, then Infer wouldn't warn here since it is already gated/config-checked.


The analysis is inter-procedural: we can analyze not only a single procedure but all its callees. For instance,
if we were to modify v1 to v3 by calling `goo()` in `foo()` as follows,
```java
 void foo(){
   // ....
   goo(); // added
  }
```
then our analysis can also detect this.

Currently, the analysis supports both Objective-C and Java but not C++.




