This error type is reported only in Objective-C. It is similar to Null
dereference, but Infer hasn't found a whole trace where the error can happen,
but only found that a null dereference can happen if you call a method with nil
as an argument. Therefore it is only a warning. For example:

```objectivec
  -(int) foo:(A* a) {
      B b* = [a foo]; // sending a message with receiver nil returns nil
      return b->x; // dereferencing b, potential NPE if you pass nil as the argument a.
  }
```

or when the parameter is a block:

```objectivec
   -(void) foo:(void (^)(BOOL))block {
      block(YES); // calling a nil block will cause a crash.
   }
```

Possible solutions are adding a check for `nil`, or making sure that the method
is not called with `nil`. When an argument will never be `nil`, you can add the
annotation `nonnull` to the argument's type, to tell Infer (and the type
system), that the argument won't be `nil`. This will silence the warning.
