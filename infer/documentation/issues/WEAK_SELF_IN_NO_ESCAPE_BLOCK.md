This check reports when `weakSelf` (a weak pointer to `self`) is used in
a block, and this block is passed to a "no escaping" method. This means that
the block passed to that method won't be leaving the current scope, this is
marked with the annotation `NS_NOESCAPE`.

The issue here is that, because the block is "no escaping", there is no need to use
`weakSelf` and `strongSelf` but we can just use `self`. This has the advantage of
not needing to deal with the added complexity of weak pointers, and it simplifies the
code.

Example:

```objectivec
  __weak __typeof(self) weakSelf = self;
  [self foo:^() { //foo's first parameter is annotates with `NS_NOESCAPE`
      [weakSelf bar];
  }];
```

**Action**:

Replace `weakSelf` with `self`:

```objectivec
  [self foo:^() {
      [self bar];
  }];
```

*Limitations:* To keep this check simple and intra-procedural, we rely on names to find `weakSelf`:
we assume that any captured weak pointer whose name contains "self" is a weak reference to `self`.
