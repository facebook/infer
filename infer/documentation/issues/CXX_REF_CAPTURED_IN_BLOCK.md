This check flags when a C++ reference is captured in an escaping block.
This means that the block will be leaving the current scope, i.e. it is
not annotated with `__attribute__((noescape))`.

Example:

```
- (void)ref_captured_in_escaping_block_bad:(int&)y {
  dispatch_async(dispatch_get_main_queue(), ^{
    int a = y;
    ...
  });
  ...;
}
```

This could cause crashes because C++ references are not managed pointers
(like ARC pointers) and so the referent is likely to be gone if the block
dereferences it later.
