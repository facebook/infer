When a block captures `weakSelf` in the following pattern:

```
__weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    int y = strongSelf->x;
```

the variable `strongSelf` should be checked for `null` before being used,
otherwise this could cause a crash because the weak pointer `weakSelf` could be
`null`.
