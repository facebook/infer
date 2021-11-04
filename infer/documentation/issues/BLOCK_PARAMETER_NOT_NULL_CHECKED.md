This error type is reported only in Objective-C/Objective-C++. It happens when the parameter is a block:

```objectivec
   -(void) foo:(void (^)(BOOL))block {
      block(YES); // calling a nil block will cause a crash.
   }
```

Possible solutions are adding a check for `nil`, or making sure that the method
is not called with `nil`. When an argument will never be `nil`, you can add the
annotation `nonnull` to the argument's type, to tell Infer (and the type
system), that the argument won't be `nil`. This will silence the warning.
