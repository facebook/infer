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
