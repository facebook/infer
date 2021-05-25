Infer reports null dereference bugs in Java, C, C++, and Objective-C
when it is possible that the null pointer is dereferenced, leading to
a crash.

### Null dereference in Java

Many of Infer's reports of potential Null Pointer Exceptions (NPE) come from code of the form

```java
  p = foo(); // foo() might return null
  stuff();
  p.goo();   // dereferencing p, potential NPE
```

If you see code of this form, then you have several options.

**If you are unsure whether or not `foo()` will return null**, you should
ideally either

1. Change the code to ensure that `foo()` can not return null, or

2. Add a check that `p` is not `null` before dereferencing `p`.

Sometimes, in case (2) it is not obvious what you should do when `p`
is `null`. One possibility is to throw an exception, failing early but
explicitly. This can be done using `checkNotNull` as in the following
code:

```java
// code idiom for failing early
import static com.google.common.base.Preconditions.checkNotNull;

  //... intervening code

  p = checkNotNull(foo()); // foo() might return null
  stuff();
  p.goo(); // p cannot be null here
```

The call `checkNotNull(foo())` will never return `null`: if `foo()`
returns `null` then it fails early by throwing a Null Pointer
Exception.

Facebook NOTE: **If you are absolutely sure that foo() will not be
null**, then if you land your diff this case will no longer be
reported after your diff makes it to master.

### Null dereference in C

Here is an example of an inter-procedural null dereference bug in C:

```c
struct Person {
  int age;
  int height;
  int weight;
};
int get_age(struct Person *who) {
  return who->age;
}
int null_pointer_interproc() {
  struct Person *joe = 0;
  return get_age(joe);
}
```

### Null dereference in Objective-C

In Objective-C, null dereferences are less common than in Java, but they still
happen and their cause can be hidden. In general, passing a message to nil does
not cause a crash and returns `nil`, but dereferencing a pointer directly does
cause a crash as well as calling a `nil` block.

```objectivec
-(void) foo:(void (^)())callback {
    callback();
}

-(void) bar {
    [self foo:nil]; //crash
}
```

Moreover, there are functions from the libraries that do not allow `nil` to be
passed as argument. Here are some examples:

```objectivec
-(void) foo {
    NSString *str = nil;
    NSArray *animals = @[@"horse", str, @"dolphin"]; //crash
}

-(void) bar {
  CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB(); //can return NULL
  ...
  CFRelease(colorSpace); //crashes if called with NULL
}
```
