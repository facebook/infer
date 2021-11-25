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
