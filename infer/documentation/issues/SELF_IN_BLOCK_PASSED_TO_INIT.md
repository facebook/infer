This check flags when `self` is captured in a block that is passed to an initialiser method. That
could cause retain cycles if the initialiser code retains the block.

Example:

```objectivec
  [obj initWithHandler:^() {
    [self foo];
    ...
  }];
```

Instead it's better to use the `weakSelf`/`strongSelf` pattern.

```objectivec
  __weak __typeof(self) weakSelf = self;
  [obj initWithHandler:^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf) {
        [strongSelf foo];
    }
    ...
  }];
```
