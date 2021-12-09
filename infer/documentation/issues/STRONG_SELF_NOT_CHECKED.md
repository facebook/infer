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
