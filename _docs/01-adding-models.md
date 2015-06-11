---
id: adding-models
title: Adding models
layout: docs
permalink: /docs/adding-models.html
section: User Guide
section_order: 01
order: 07
---

## Why do we need models

When analyzing projects with call dependencies between functions, Infer follows the call graph to decide in which order analyze these functions. The main goal is to use the analysis summary of a function wherever this function is called. On the following example:

```C
int foo(int x) {
  if (x < 42) {
    return x;
  } else {
    return 0;
  }
}

int bar() {
  return foo(24);
}

int baz() {
  return foo(54);
}
```

Infer starts with the analysis on `foo` and detect that this function either returns `0` if the argument is greater than or equal to `42`, or returns the value of the argument otherwise. With this information, Infer detects that `bar` always returns `24` and `baz` always returns `0`.

Now, it may happen that the code of some function is not available during the analysis. For example, this happens when a project uses pre-compiled libraries. The most typical case is the use of the standard library like in the following example:

```C
#include <stdlib.h>

int* create() {
  int *p = malloc(sizeof(int));
  if (p == NULL) exit(1);
  return p;
}

void assign(int x, int *p) {
  *p = x;
}

int* my_function() {
  int *p = create();
  assign(42, p);
  return p;
}
```

Here, Infer will start with the analysis of `create` but will not find the source code for `malloc`. To deal with this situation, Infer relies on models of the missing functions to proceed with the analysis. The function `malloc` is internally modeled as either returning `NULL`, or returning a valid and allocated pointer. Similarly, the function `exit` is modeled as terminating the execution. Using these two models, Infer detects that `create` always returns an allocated pointer and that `my_function` is safe.

At this point, it is important to note that missing source code and missing models do not make the analysis fail. Missing functions are treated as having no effect. However, even though skipping these missing functions is fine in most cases, there can be cases where it affects the quality of the analysis. For example, missing models can lead to incorrect bug reports.

Consider the case of a function `lib_exit` having the same semantic as `exit` but defined in an pre-compiled library not part of the project being analyzed:

```C
void lib_exit(int);

int* create() {
  int *p = malloc(sizeof(int));
  if (p == NULL) lib_exit(1);
  return p;
}
```

In this case, Infer will not be able to know that the return statement is only possible in the case where `p` is not null. When analyzing `my_function`, Infer will consider the null case and report a null dereference error in the call to `assign(42, p)`.

Similarly, considering a function `lib_alloc` equivalent to `malloc`, and the function `create` now defined as:

```C
int* lib_alloc(int);

int* create() {
  int *p = lib_alloc(sizeof(int));
  return p;
}
```

Then Infer will not report any null dereference in `my_function`.

## Examples of models

### Some models for C

Adding new models is easy. The models for C can be found in [`infer/models/c/src/`](https://github.com/facebook/infer/tree/master/infer/models/c/src). The file [`libc_basic.c`](https://github.com/facebook/infer/blob/master/infer/models/c/src/libc_basic.c) contains models for some of the most commonly encountered functions from the C standard library. For example, the function `xmalloc`, which is essentially the same function as `create` defined above, is modeled by:

```C
void *xmalloc(size_t size) {
  void *ret = malloc(size);
  INFER_EXCLUDE_CONDITION(ret == NULL);
  return ret;
}
```

The function `xmalloc` is modeled using `malloc` to create an allocated object and the macro `INFER_EXCLUDE_CONDITION` used to eliminate the case where `malloc` can return null. The list of helper functions and macros for writing models can be found in [`infer_builtins.c`](https://github.com/facebook/infer/blob/master/infer/models/c/src/infer_builtins.c).

For a slightly more complex example, `realloc` is modeled as:

```C
void *realloc(void *ptr, size_t size) {
  if(ptr==0) { // if ptr in NULL, behave as malloc
    return malloc(size);
  }
  int old_size;
  int can_enlarge;
  old_size = __get_array_size(ptr); // force ptr to be an array
  can_enlarge = __infer_nondet_int(); // nondeterministically choose whether the current block can be enlarged
  if(can_enlarge) {
    __set_array_size(ptr, size); // enlarge the block
    return ptr;
  }
  int *newblock = malloc(size);
  if(newblock) {
    free(ptr);
    return newblock;
  }
  else { // if new allocation fails, do not free the old block
    return newblock;
  }
}
```

This model is based on existing models for `malloc` and `free` and three helper functions:

- `__get_array_size(ptr)` which allows to manipulate with a model what Infer knows about the size of the allocated memory
- `__set_array_size(ptr, size)` to modify the information about the size of the allocated memory
- `__infer_nondet_int()` to create a variable which can have any possible integer value

### For Java

The models for Java are following the same approach and the list of helper functions is in:

  [`infer/models/java/src/com/facebook/infer/models/InferBuiltins.java`](https://github.com/facebook/infer/blob/master/infer/models/java/src/com/facebook/infer/models/InferBuiltins.java)
  [`infer/models/java/src/com/facebook/infer/models/InferUndefined.java`](https://github.com/facebook/infer/blob/master/infer/models/java/src/com/facebook/infer/models/InferUndefined.java)

For example, Infer treats Java hash maps using a recency abstraction model: Infer remembers the last two keys being added by `put` and checked by `containsKey`, which can be used to make sure that no null pointer exceptions are coming from the fact that `get(key)` returns null when `key` is not not in the map. This behavior can just be implemented via a model written in Java with the help of few helper functions understood by Infer. These models can be found in:

  [`infer/models/java/src/java/util/HashMap.java`](https://github.com/facebook/infer/blob/master/infer/models/java/src/java/util/HashMap.java)

and just rely on these two methods:

- `InferUndefined.boolean_undefined()` to create a non-deterministic choice
- `(V)InferUndefined.object_undefined()` to create a non null undefined object of type `V`

## How to add new models

Let's look at a toy example in Java. As explained above, models for C, Objective-C and Java are all following the same approach.

```Java
import lib.Server;

public class Test {

  enum Status {
    SUCCESS, FAILURE, PING_FAILURE, CONNECTION_FAILURE
  }

  Status convertStatus(Server s) {
    switch (s.getStatus()) {
    case 0:
      return Status.SUCCESS;
    case 1:
      return Status.FAILURE;
    case 2:
      return Status.FAILURE;
    default: // should not happen
      return null;
    }
  }

  String statusName(Server s) {
    Status status = convertStatus(s);
    return status.name();
  }

}
```

Assuming that the class `lib.Server` is part of a pre-compiled library, Infer will report a null pointer exception in `statusName`. This happens whenever `s.getStatus()` returns a value greater that `3`, in which case the default branch of the switch statement is taken and `convertStatus` returns `null`. However, we know from the documentation that the method `lib.Server.getStatus` can only return `0`, `1`, or `2`. A possible approach would be to use an assertion like the Guava `Preconditions.checkState` to inform Infer about the invariant:

```Java
Status convertStatus(Server s) {
  int serverStatus = s.getStatus();
  Preconditions.checkState(serverStatus >= 0 && serverStatus < 3);
  switch (s.getStatus()) {
    ...
  }
}
```

However, in the case where adding preconditions is not possible, we can then write a model for `getStatus()` in order to make the analysis more precise.

To create a model for `getStatus()`, we need to add a class with the name and the same package as for the original method. In this example:

- create a file `infer/models/java/src/infer/models/Server.java` with the following content:

    ```Java
    package infer.models;

    import com.facebook.infer.models.InferBuiltins;
    import com.facebook.infer.models.InferUndefined;

    public class Server {

      public int getStatus() {
        int status = InferUndefined.int_undefined();
        InferBuiltins.assume(status >= 0 && status < 3);
        return status;
      }
    }
    ```

- recompile infer:

    ```bash
    make -C infer
    ```

- run the analysis again:

    ```bash
    infer -- javac Test.java
    ```

Now it should no longer report a null pointer exception.
