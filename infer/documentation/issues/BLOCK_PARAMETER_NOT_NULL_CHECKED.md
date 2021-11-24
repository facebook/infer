This error type is reported only in Objective-C/Objective-C++. It happens when a method has a block as a parameter,
and the block is executed in the method's body without checking it for `nil` first. If a `nil` block is passed to
the method, then this will cause a crash. For example:

```objectivec
- (void)uploadTaskWithRequest:(NSURLRequest*)urlRequest
                       fromFile:(NSURL*)fileURL
                       delegate:(id)delegate
                  delegateQueue:(NSOperationQueue*)delegateQueue
                     completion:(void (^)())completion {
     ...
    completion();
}
```

**Action**:
Possible solutions are adding a check for `nil`, or making sure that the method
is not ever called with `nil`. When an argument will never be `nil`, you can add
the annotation `nonnull` to the argument's type, to tell Infer (and the type
system), that the argument won't be `nil`. This will silence the warning.
