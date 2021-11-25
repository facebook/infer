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
