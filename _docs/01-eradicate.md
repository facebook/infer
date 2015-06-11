---
id: eradicate
title: "Infer : Eradicate"
layout: docs
permalink: /docs/eradicate.html
section: User Guide
section_order: 01
order: 04
---


> "I call it my billion-dollar mistake. It was the invention of the null reference in 1965."
> 
> [Tony Hoare](http://en.wikipedia.org/wiki/Tony_Hoare)


### What is Infer:Eradicate?

Infer:Eradicate is a type checker for @Nullable annotations for Java. It is part of the Infer static analysis suite of tools.
The goal is to eradicate null pointer exceptions.

<a href="https://developer.android.com/reference/android/support/annotation/Nullable.html">@Nullable</a>
annotations denote that a parameter, field or the return value of a method can be null.
When decorating a parameter, this denotes that the parameter can legitimately be null and the method will need to deal with it. When decorating a method, this denotes the method might legitimately return null.

Starting from @Nullable-annotated programs, the checker performs a flow sensitive analysis
to propagate the nullability through assignments and calls, and flags errors for
unprotected accesses to nullable values or inconsistent/missing annotations.
It can also be used to add annotations to a previously un-annotated program.

### What is the @Nullable convention?

If you say nothing, you're saying that the value cannot be null. This is the recommended option when possible:

Program safely, annotate nothing!

When this cannot be done, add a @Nullable annotation before the type to indicate that the value can be null.

### What is annotated?

Annotations are placed at the interface of method calls and field accesses:

- Parameters and return type of a method declaration.
- Field declarations.

Local variable declarations are not annotated: their nullability is inferred.

### How is Infer:Eradicate invoked?

Eradicate can be invoked by adding the option `-a eradicate` to the analysis command as in this example:

```bash
infer -a eradicate -- javac Test.java
```

The checker will report an error on the following program that accesses a nullable value without null check:

```java
class C {
  int getLength(@Nullable String s) {
    return s.length();
  }
}
```

But it will not report an error on this guarded dereference:

```java
class C {
  int getLength(@Nullable String s) {
    if (s != null) {
      return s.length();
    } else {
      return -1;
    }
  }
}
```

Eradicate reports the following [warnings](/docs/eradicate-warnings.html).
