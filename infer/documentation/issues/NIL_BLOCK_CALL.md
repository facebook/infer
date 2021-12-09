This check reports when one tries to call an Objective-C block that is `nil`.
This causes a crash.

Example:

```objectivec
-(void) foo:(void (^)())callback {
    callback();
}

-(void) bar {
    [self foo:nil]; //crash
}
```

**Action**:

Adding a check for `nil` before calling the block, or making sure never to call the method `foo:` with `nil`.
