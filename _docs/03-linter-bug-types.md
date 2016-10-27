---
docid: linters-bug-types
title: Linters bug types
layout: docs
permalink: /docs/linters-bug-types.html
---

Here is an overview of the linter checks we provide in Infer:

- Issues reported in iOS apps
  - [Assign pointer warning](/docs/linters-bug-types.html#ASSIGN_POINTER_WARNING)
  - [Bad pointer comparison](/docs/linters-bug-types.html#BAD_POINTER_COMPARISON)
  - [C++ reference captured in Objective-C block](/docs/linters-bug-types.html#CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK)
  - [Direct atomic property access](/docs/linters-bug-types.html#DIRECT_ATOMIC_PROPERTY_ACCESS)
  - [Global variable initialized with function or method call](/docs/linters-bug-types.html#GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL)
  - [Registered observer being deallocated](/docs/linters-bug-types.html#REGISTERED_OBSERVER_BEING_DEALLOCATED)
  - [Strong delegate warning](/docs/linters-bug-types.html#STRONG_DELEGATE_WARNING)
  
- Issues reported in iOS code about [ComponentKit](http://componentkit.org)
  - [Component factory funciton](/docs/linters-bug-types.html#COMPONENT_FACTORY_FUNCTION)
  - [Component initializer with side-effects](/docs/linters-bug-types.html#COMPONENT_INITIALIZER_WITH_SIDE_EFFECTS)
  - [Component with multiple factory functions](/docs/linters-bug-types.html#COMPONENT_WITH_MULTIPLE_FACTORY_FUNCTIONS)
  - [Component with unconventional superclass](/docs/linters-bug-types.html#COMPONENT_WITH_UNCONVENTIONAL_SUPERCLASS)
  - [Mutable Local Variable in component file](/docs/linters-bug-types.html#MUTABLE_LOCAL_VARIABLE_IN_COMPONENT_FILE)


 
## <a name="ASSIGN_POINTER_WARNING"></a> Assign pointer warning

This check fires when a pointer to an Obj-C object is tagged with an `assign` property (similar to the `-Warc-unsafe-retained-assign` compiler flag). Not holding a strong reference to the object makes it easy to accidentally create and use a dangling pointer.

## <a name="BAD_POINTER_COMPARISON"></a> Bad pointer comparison

Infer reports these warnings in Objective-C when a boxed primitive type such as `NSNumber *` is coerced to a boolean in a comparison. For example, consider the code

```objc
void foo(NSNumber * n) {
  if (n) ...
```

The branch in the above code will be taken when the pointer `n` is non-`nil`, but the programmer might have actually wanted the branch to be taken when the integer pointed to by `n` is nonzero (e.g., she may have meant to call an accessor like `[n intValue]` instead). Infer will ask the programmer explicitly compare `n` to `nil` or call an accessor to clarify her intention.

## <a name="CXX_REFERENCE_CAPTURED_IN_OBJC_BLOCK"></a> C++ reference captured in Objective-C block

With this check, Infer detects C++ references captured in a block. Doing this is almost always wrong.
The reason is that C++ references are not managed pointers (like ARC pointers) and so the referent is
likely to be gone by the time the block gets executed.
One solution is to do a local copy of the reference and pass that to the block. Example:

```c
(int &) v;
...
const int copied_v = v;
^{
// use copied_v not v
};
```


## <a name="DIRECT_ATOMIC_PROPERTY_ACCESS"></a> Direct atomic property access

This check warns you when you are accessing an atomic property directly with an ivar.
This makes the atomic property not atomic anymore. So potentially you may get a race condition.

To fix the problem you need to access properties with their getter or setter.

## <a name="GLOBAL_VARIABLE_INITIALIZED_WITH_FUNCTION_OR_METHOD_CALL"></a> Global variable initialized with function or method call

This checker warns you when the initialization of global variable contain a method or function call.
The warning wants to make you aware that some functions are expensive.
As the global variables are initialized before main() is called, these initializations can slow down the
start-up time of an app.

## <a name="REGISTERED_OBSERVER_BEING_DEALLOCATED"></a> Registered observer being deallocated

Objects register with a notification center to receive notifications.
This check warns you when an object is registered as observer of a NSNotificationCenter but
it is never unregistered. This is problematic as if the object is not unregistered
the notification center can still send notification even after the object has been deallocated.
In that case we would get a crash.

## <a name="STRONG_DELEGATE_WARNING"></a> Strong delegate warning

This check warns you when you have a property called delegate or variations thereof which is declared strong. The idea is that
delegates should generally be weak, otherwise this may cause retain cycles.

## <a name="COMPONENT_FACTORY_FUNCTION"></a> Component factory function

[Doc in ComponentKit page](http://componentkit.org/docs/break-out-composites.html)

## <a name="COMPONENT_INITIALIZER_WITH_SIDE_EFFECTS"></a> Component initializer with side-effects

[Doc in ComponentKit page](http://componentkit.org/docs/no-side-effects.html)

## <a name="COMPONENT_WITH_MULTIPLE_FACTORY_FUNCTIONS"></a> Component with multiple factory functions

[Doc in ComponentKit page](http://componentkit.org/docs/avoid-overrides.html)


## <a name="COMPONENT_WITH_UNCONVENTIONAL_SUPERCLASS"></a> Component with unconventional superclass

[Doc in ComponentKit page](http://componentkit.org/docs/never-subclass-components.html)

## <a name="MUTABLE_LOCAL_VARIABLE_IN_COMPONENT_FILE"></a> Mutable local variable in component file

[Doc in ComponentKit page](http://componentkit.org/docs/avoid-local-variables.html)










