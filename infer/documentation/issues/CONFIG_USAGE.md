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
