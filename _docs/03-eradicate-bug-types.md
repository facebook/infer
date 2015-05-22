---
id: eradicate-bug-types
title: Eradicate @Nullable Checker
layout: docs
permalink: /docs/eradicate-bug-types.html
section: Bug Types Reference
section_order: 02
order: 00
---

> "I call it my billion-dollar mistake. It was the invention of the null reference in 1965."
> 
> [Tony Hoare](http://en.wikipedia.org/wiki/Tony_Hoare)


##What is Infer:Eradicate?

Infer:Eradicate is a type checker for @Nullable annotations for Java. It is part of the Infer static analysis suite of tools.
The goal is to eradicate null pointer exceptions.

Starting from @Nullable-annotated programs, the checker performs a flow sensitive analysis
to propagate the nullability through assignments and calls, and flags errors for
unprotected accesses to nullable values or inconsistent/missing annotations.
It can also be used to add annotations to a previously un-annotated program.

##What is the @Nullable convention?

If you say nothing, you're saying that the value cannot be null. This is the recommended option when possible:

Program safely, annotate nothing!

When this cannot be done, add a @Nullable annotation before the type to indicate that the value can be null.

##What is annotated?

Annotations are placed at the interface of method calls and field accesses:

- Parameters and return type of a method declaration.

- Field declarations.

Local variable declarations are not annotated: their nullability is inferred.

##How is Infer:Eradicate invoked?

Infer:Eradicate can be invoked by adding the option ```-a eradicate``` to the analysis command as in this example:

```infer -a eradicate -- javac Test.java```

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


##What warnings are reported by Eradicate?
Eradicate reports the following warnings:

### ERADICATE\_NULL\_FIELD\_ACCESS

A field access of the form x.field where x could be null.

Example:

```java
class C {
  void foo(@Nullable C x) {
    x.field = 3;
  }
}
```

Action:
Make sure that x cannot be null by changing the code or changing annotations.
If this cannot be done, the only choice is to use defensive programming:
  if (x != null) { ... x.field ... }
  else {  ... you need to decide what to do when x is null ... }
The general recommendation is to push null checks up the call chain as much as possible,
to detect the place where null values originate and deal with them at that point.
When a null value is propagated down the call chain it is often difficult to determine
its origin without global knowledge of what the program does.
For example, a null value could originate in third party libraries which are not under your
control, and the best place to check for null is typically immediately after calling these
library functions.

###ERADICATE\_NULL\_METHOD\_CALL
A method call x.m(...) where x could be null.

Example:

```java
class C {
  void foo(@Nullable C x) {
    String s = x.toString();
  }
}
```
Action:
Same as for NULL\_FIELD\_ACCESS.

###ERADICATE\_FIELD\_NOT\_NULLABLE
An  assignment x.f = v where v could be null and field f is not annotated with @Nullable.

Example:

```java
class C {
  String f;

  void foo(@Nullable String s) {
    f = s;
  }
}
```

Action:
The preferred action is to ensure that a null value is never stored in the field,
by changing the code or changing annotations.
If this cannot be done, add a @Nullable annotation to the field. This annotation might trigger
more warnings in other code that uses the field, as that code must now deal with null values.

###ERADICATE\_FIELD\_NOT\_INITIALIZED
The constructor does not initialize a field f which is not annotated with @Nullable

Example:

```java
class C {
  String f;

  C () { // field f not initialized and not annotated @Nullable
  }
}
```

Action:
The preferred action is to initialize the field with a value that is not null.
If, by design, null is a valid value for the field, then it should be annotated with @Nullable.

###ERADICATE\_PARAMETER\_NOT\_NULLABLE
Method call x.m(..., v, ...) where v can be null and the corresponding parameter in method m is not annotated with @Nullable

Example:

```java
class C {
  void m(C x) {
    String s = x.toString()
  }

  void test(@Nullable C x) {
    m(x);
  }
}
```

Action:
The preferred action is to ensure that a null value is never passed to the method,
by changing the code or changing annotations.
If this cannot be done, add a @Nullable annotation to the relevant parameter in the method declaration.
This annotation might trigger more warnings in the implementation of method m, as that code must now deal with null values.

###ERADICATE\_RETURN\_NOT\_NULLABLE
Method m can return null, but the method's return type is not annotated with @Nullable

Example:

```java
class C {
  String m() {
    return null;
  }
}
```

Action:
The preferred action is to ensure that a null value is never returned by the method,
by changing the code or changing annotations.
If this cannot be done, add a @Nullable annotation to the the method declaration.
This annotation might trigger more warnings in the callers of method m, as the callers must now deal with null values.


###ERADICATE\_INCONSISTENT\_SUBCLASS\_RETURN\_ANNOTATION
The return type of the overridden method is annotated @Nullable, but the corresponding method in the superclass is not.

Action: choose a consistent annotation based on the desired invariant.

Example:

```java
class A {
  String create() {
    return new String("abc");
  }
}

class B extends A {
  @Nullable String create() {  // Inconsistent @Nullable annotation.
      return null;
  }
}
```
A consistent use of @Nullable on the return type across subtying should prevent runtime issue like in:

````java
class Main {

  int foo(A a) {
     String s = a.create();
     return s.length();
  }

  void main(String[] args) {
     A a = new B();
     foo(a);
  }

}
```

###ERADICATE\_INCONSISTENT\_SUBCLASS\_PARAMETER\_ANNOTATION

A parameter of the overridden method is missing a @Nullable annotation present in the superclass.

Action: choose a consistent annotation based on the desired invariant.

Example:

```java
class A {

  int len(@Nullable String s) {
    if (s != null) {
      return s.length();
    } else {
      return 0;
    }
  }
}

class B extends A {

  int len(String s) {  // @Nullable missing.
    return s.length();
  }
}
```

A consistent use of @Nullable on paramters across subtying should prevent runtime issue like in:

```java
public class Main {

  String s;

  int foo() {
    A a = new B();
    return a.len(s);
  }
}
```

###ERADICATE\_CONDITION\_REDUNDANT (inactive by default)
Condition (x != null) or (x == null) when x cannot be null: the first condition is always true and the second is always false

Example:

```java
class C {
  void m() {
    String s = new String("abc");
    if (s != null) {
      int n = s.length();
    }
  }
}
```

Action:
Make sure that the annotations are correct, as the condition is considered redundant based on the existing annotations.
In particular, check the annotation of any input parameters and fields of the current method, as well
as the annotations of any method called directly by the current method, if relevant.
If the annotations are correct, you can remove the redundant case.

###ERADICATE\_RETURN\_OVER\_ANNOTATED (inactive by default)
Method m is annotated with @Nullable but the method cannot return null

Example:

```java
class C {
  @Nullable String m() {
    String s = new String("abc");
    return s;
  }
}
```

Action:
Make sure that the annotations are correct, as the return annotation is considered redundant based on the existing annotations.
In particular, check the annotation of any input parameters and fields of the current method, as well
as the annotations of any method called directly by the current method, if relevant.
If the annotations are correct, you can remove the @Nullable annotation.
