This error type is only reported in Objective-C. This is similar to Null
dereference, but Infer hasn't found a whole trace where the error can happen,
but only found that a null dereference can happen if an instance variable of a
parameter is `nil`. For example:

```objectivec
  -(int) foo {
      B b* = [self->_a foo]; // sending a message with receiver nil returns nil
      return b->x; // dereferencing b, potential NPE if you pass nil as the argument a.
  }
```

Possible solutions are adding a check for `nil`, or making sure that the method
is not called with `nil`.
