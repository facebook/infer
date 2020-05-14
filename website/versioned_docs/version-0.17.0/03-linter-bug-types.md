---
id: linters-bug-types
title: Linters bug types
---

Here is an overview of the linter checks we provide in Infer:

## Assign pointer warning

This check fires when a pointer to an Obj-C object is tagged with an `assign`
property (similar to the `-Warc-unsafe-retained-assign` compiler flag). Not
holding a strong reference to the object makes it easy to accidentally create
and use a dangling pointer.

## Bad pointer comparison

Infer reports these warnings in Objective-C when a boxed primitive type such as
`NSNumber *` is coerced to a boolean in a comparison. For example, consider the
code

```objectivec
void foo(NSNumber * n) {
  if (n) ...
```

The branch in the above code will be taken when the pointer `n` is non-`nil`,
but the programmer might have actually wanted the branch to be taken when the
integer pointed to by `n` is nonzero (e.g., she may have meant to call an
accessor like `[n intValue]` instead). Infer will ask the programmer explicitly
compare `n` to `nil` or call an accessor to clarify her intention.

## C++ reference captured in Objective-C block

With this check, Infer detects C++ references captured in a block. Doing this is
almost always wrong. The reason is that C++ references are not managed pointers
(like ARC pointers) and so the referent is likely to be gone by the time the
block gets executed. One solution is to do a local copy of the reference and
pass that to the block. Example:

```c
(int &) v;
...
const int copied_v = v;
^{
// use copied_v not v
};
```

## Direct atomic property access

This check warns you when you are accessing an atomic property directly with an
ivar. This makes the atomic property not atomic anymore. So potentially you may
get a race condition.

To fix the problem you need to access properties with their getter or setter.

## Global variable initialized with function or method call

This checker warns you when the initialization of global variable contain a
method or function call. The warning wants to make you aware that some functions
are expensive. As the global variables are initialized before main() is called,
these initializations can slow down the start-up time of an app.

## Registered observer being deallocated

Objects register with a notification center to receive notifications. This check
warns you when an object is registered as observer of a NSNotificationCenter but
it is never unregistered. This is problematic as if the object is not
unregistered the notification center can still send notification even after the
object has been deallocated. In that case we would get a crash.

## Strong delegate warning

This check warns you when you have a property called delegate or variations
thereof which is declared strong. The idea is that delegates should generally be
weak, otherwise this may cause retain cycles.

## Unavailable api in supported ios sdk

This checks warns you when you are using an API (constant, method call, etc.)
that is only defined in a version higher than the version that you support. To
enable this check, pass to Infer the option
`--iphoneos-target-sdk-version version`. Calling an undefined API will lead to a
crash in the app. To fix this, you can choose a different API or use it inside
an if, as in:

```objectivec
if ([UIFont respondsToSelector:@selector(systemFontOfSize:weight:)]) {
  font = [UIFont systemFontOfSize:size weight:0];
}
```

or

```objectivec
if (kCFCoreFoundationVersionNumber >= kCFCoreFoundationVersionNumber_iOS_9_0) {
  font = [UIFont systemFontOfSize:size weight:0];
}
```

## Pointer To const Objective-C Class

In Objective-C, `const Class *` represents a mutable pointer pointing to an
Objective-C class where the ivars cannot be changed. More useful is
`Class *const` instead, meaning the destination of the pointer cannot be
changed.

## Objective-C Weak Property has Custom Setter

This check warns you when you have a custom setter for a weak property. When
compiled with Automatic Reference Counting (ARC, `-fobj-arc`) ARC may set the
property to `nil` without invoking the setter, for example:

```objectivec
#import <Foundation/Foundation.h>

@interface Employee : NSObject {
  NSString* _name;
  __weak Employee* _manager;
}
-(id)initWithName:(NSString*)name;
@property(atomic, weak) Employee* manager;
-(void)report;
@end

@implementation Employee

-(id)initWithName:(NSString*)name {
  _name = name;
  return self;
}

-(NSString*)description {
  return _name;
}

-(void)report {
  NSLog(@"I work for %@", _manager);
}

-(Employee*)manager {
  return _manager;
}

// DON'T do this; ARC will not call this when setting _manager to nil.
-(void)setManager:(Employee*)newManager {
  NSLog(@"Meet the new boss...");
  _manager = newManager;
}

@end

int main(int argc, char *argv[])
{
  Employee* bob = [[Employee alloc] initWithName:@"Bob"];
  Employee* sue = [[Employee alloc] initWithName:@"Sue"];
  bob.manager = sue;
  [bob report];
  sue = nil;
  [bob report];
  return 0;
}
```

This prints:

```
Meet the new boss...
I work for Sue
I work for (null)
```

Note that the custom setter was only invoked once.

## Component factory function

[Doc in ComponentKit page](http://componentkit.org/docs/break-out-composites)

## Component initializer with side effects

[Doc in ComponentKit page](http://componentkit.org/docs/no-side-effects)

## Component with multiple factory methods

[Doc in ComponentKit page](http://componentkit.org/docs/avoid-overrides)

## Component with unconventional superclass

[Doc in ComponentKit page](http://componentkit.org/docs/never-subclass-components)

## Mutable local variable in component file

[Doc in ComponentKit page](http://componentkit.org/docs/avoid-local-variables)
