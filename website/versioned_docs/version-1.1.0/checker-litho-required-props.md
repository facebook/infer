---
title: "Litho \"Required Props\""
description: "Checks that all non-optional `@Prop`s have been specified when constructing Litho components."
---

Checks that all non-optional `@Prop`s have been specified when constructing Litho components.

Activate with `--litho-required-props`.

Supported languages:
- C/C++/ObjC: No
- Java: Yes
- C#/.Net: Yes

This analysis checks that all non-optional [`@Prop`](https://fblitho.com/docs/props)`s have been specified when constructing Litho components. This is a [Litho](https://fblitho.com/) specific checker.


## What are required Props?
In a nutshell, a Litho Component is essentially a class that defines immutable inputs, called prop (annotated with `@Prop`) in component hierarchy methods. For each Component there is a corresponding spec class which defines the required props:. E.g:

```java
class MyComponentSpec {

  static void onCreate(
      ComponentContext c,
      @Prop(optional = true) String prop1, @Prop int prop2) {
    ...
  }
  ...
}
```

`MyComponentSpec` defines two props: a String prop called `prop1` and an int prop named `prop2`. For each prop defined on the spec, the annotation processor creates a builder pattern method that has the same name as the prop.

Developers pass down values for these props by calling the appropriate methods:

```java
MyComponent.create(c)
    .prop1("My prop 1")
    .prop2(256)
    .build();
```

If the required props are not called, then annotation processor throws an exception in run time. This is really bad and that's where this checker comes into play to detect such cases statically.


## Examples

E.g. the following is caught as [MISSING_REQUIRED_PROP](/docs/1.1.0/all-issue-types#missing_required_prop) `prop2`.

```java
MyComponent.create(c)
    .prop1("My prop 1")
    .build();
```

The following is ok though since `prop1` is optional.

```java
MyComponent.create(c)
    .prop2(8)
    .build();
```

Note that, the functions `create()` and `build()` could be defined in different methods and there could be various function calls, aliasing, and control flow patterns in between. Hence, this checker is inter-procedural.


## List of Issue Types

The following issue types are reported by this checker:
- [MISSING_REQUIRED_PROP](/docs/1.1.0/all-issue-types#missing_required_prop)
