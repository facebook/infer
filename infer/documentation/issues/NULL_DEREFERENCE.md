Infer reports null dereference bugs in C, Objective-C and Java. The issue is
about a pointer that can be `null` and it is dereferenced. This leads to a crash
in all the above languages.

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
cause a crash as well as calling a `nil` block.C

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

### Null dereference in Java

Many of Infer's reports of potential NPE's come from code of the form

```java
  p = foo(); // foo() might return null
  stuff();
  p.goo();   // dereferencing p, potential NPE
```

If you see code of this form, then you have several options.

<b> If you are unsure whether or not foo() will return null </b>, you should
ideally i. Change the code to ensure that foo() can not return null ii. Add a
check for whether p is null, and do something other than dereferencing p when it
is null.

Sometimes, in case ii it is not obvious what you should do when p is null. One
possibility (a last option) is to throw an exception, failing early. This can be
done using checkNotNull as in the following code:

```java
  // code idiom for failing early

  import static com.google.common.base.Preconditions.checkNotNull;

  //... intervening code

  p = checkNotNull(foo()); // foo() might return null
  stuff();
  p.goo();   // dereferencing p, potential NPE
```

The call checkNotNull(foo()) will never return null; in case foo() returns null
it fails early by throwing an NPE.

<b> If you are absolutely sure that foo() will not be null </b>, then if you
land your diff this case will no longer be reported after your diff makes it to
master. In the future we might include analysis directives (hey, analyzer, p is
not null!) like in Hack that tell the analyzer the information that you know,
but that is for later.
