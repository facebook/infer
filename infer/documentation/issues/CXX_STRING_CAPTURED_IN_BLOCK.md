This check flags when a local variable of type `std::string` is captured in an escaping block.
This means that the block will be leaving the current scope, i.e. it is
not annotated with `__attribute__((noescape))`.

Example:

```
- (void)string_captured_in_escaping_block_bad {
  std::string fullName;
  dispatch_async(dispatch_get_main_queue(), ^{
    const char* c = fullName.c_str();
    ...
  });
  ...;
}
```

This could cause crashes because the variable is likely to be freed if the block
uses it later.
