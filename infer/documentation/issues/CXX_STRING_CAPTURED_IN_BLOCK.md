This check flags when an internal pointer of a local variable of type `std::string` is captured in an escaping block.
This means that the block will be leaving the current scope, i.e. it is
not annotated with `__attribute__((noescape))`.

Example:

```
  std::string fullName;
  const char* c = fullName.c_str();
  dispatch_async(dispatch_get_main_queue(), ^{
    const char* c1 = c;
  });
```

This could cause crashes because the variable is likely to be freed when the code is executed, leaving the pointer dangling.
