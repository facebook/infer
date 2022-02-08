### Memory leak in C

This error type is only reported in C and Objective-C code. In Java we do not
report memory leaks because it is a garbage collected language.

In C, Infer reports memory leaks when objects are created with `malloc` and not
freed. For example:

```c
-(void) memory_leak_bug {
    struct Person *p = malloc(sizeof(struct Person));
}
```

### Memory leak in Objective-C

Additionally, in Objective-C, Infer reports memory leaks that happen when
objects from Core Foundation or Core Graphics don't get released.

```objectivec
-(void) memory_leak_bug_cf {
    CGPathRef shadowPath = CGPathCreateWithRect(self.inputView.bounds, NULL); //object created and not released.
}
```
