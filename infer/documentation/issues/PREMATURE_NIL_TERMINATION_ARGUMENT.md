This error type is reported in C and Objective-C. In many variadic methods,
`nil` is used to signify the end of the list of input objects. This is similar
to nil-termination of C strings. If one of the arguments that is not the last
argument to the method is `nil` as well, Infer reports an error because that may
lead to unexpected behavior.

An example of such variadic methods is
[arrayWithObjects](https://developer.apple.com/library/prerelease/ios/documentation/Cocoa/Reference/Foundation/Classes/NSArray_Class/index.html#//apple_ref/occ/clm/NSArray/arrayWithObjects)

```objectivec
  NSArray *foo = [NSArray arrayWithObjects: @"aaa", str, @"bbb", nil];
```

In this example, if `str` is `nil` then an array `@[@"aaa"]` of size 1 will be
created, and not an array `@[@"aaa", str, @"bbb"]` of size 3 as expected.
