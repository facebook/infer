---
id: eradicate-warnings
title: Eradicate warnings
layout: docs
permalink: /docs/eradicate-warnings.html
section: Bug Types Reference
section_order: 03
order: 03
---

Below you will find a description of all the warnings reported by 
[Eradicate](/docs/eradicate.html).

- [Eradicate null field access](/docs/eradicate-warnings.html#ERADICATE_NULL_FIELD_ACCESS)
- [Eradicate null method call](/docs/eradicate-warnings.html#ERADICATE_NULL_METHOD_CALL)
- [Eradicate field not nullable](/docs/eradicate-warnings.html#ERADICATE_FIELD_NOT_NULLABLE)
- [Eradicate field not initialized](/docs/eradicate-warnings.html#ERADICATE_FIELD_NOT_INITIALIZED)
- [Eradicate parameter not nullable](/docs/eradicate-warnings.html#ERADICATE_PARAMETER_NOT_NULLABLE)
- [Eradicate return not nullable](/docs/eradicate-warnings.html#ERADICATE_RETURN_NOT_NULLABLE)
- [Eradicate condition redundant](/docs/eradicate-warnings.html#ERADICATE_CONDITION_REDUNDANT)
- [Eradicate return over annotated](/docs/eradicate-warnings.html#ERADICATE_RETURN_OVER_ANNOTATED)
- [Eradicate inconsistent subclass return annotation](/docs/eradicate-warnings.html#ERADICATE_INCONSISTENT_SUBCLASS_RETURN_ANNOTATION)
- [Eradicate inconsistent subclass parameter annotation](/docs/eradicate-warnings.html#ERADICATE_INCONSISTENT_SUBCLASS_PARAMETER_ANNOTATION)


## <a name="ERADICATE_NULL_FIELD_ACCESS"></a>Eradicate null field access

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
The general recommendation is to push null checks up the call chain as much as possible in order
to detect the place where null values originate and deal with them at that point.
When a null value is propagated down the call chain it is often difficult to determine
its origin without global knowledge of what the program does.
For example, a null value could originate in third party libraries which are not under your
control, and the best place to check for null is typically immediately after calling these
library functions.

## <a name="ERADICATE_NULL_METHOD_CALL"></a>Eradicate null method call

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
Same as for Null field access.

## <a name="ERADICATE_FIELD_NOT_NULLABLE"></a>Eradicate field not nullable


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

## <a name="ERADICATE_FIELD_NOT_INITIALIZED"> </a>Eradicate field not initialized


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



## <a name="ERADICATE_PARAMETER_NOT_NULLABLE"> </a>Eradicate parameter not nullable

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


## <a name="ERADICATE_RETURN_NOT_NULLABLE"> </a>Eradicate return not nullable

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

## <a name="ERADICATE_CONDITION_REDUNDANT"> </a>Eradicate condition redundant


This report is inactive by default. Condition (x != null) or (x == null) when x cannot be null: the first condition is always true and the second is always false

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



## <a name="ERADICATE_RETURN_OVER_ANNOTATED"> </a>Eradicate return overannotated

This report is inactive by default. Method m is annotated with @Nullable but the method cannot return null

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


## <a name="ERADICATE_INCONSISTENT_SUBCLASS_RETURN_ANNOTATION"> </a>Eradicate inconsistent subclass return annotation

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
A consistent use of @Nullable on the return type across subtyping should prevent runtime issue like in:

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

## <a name="ERADICATE_INCONSISTENT_SUBCLASS_PARAMETER_ANNOTATION"></a> Inconsistent subclass parameter annotation

A parameter of the overridden method is missing a @Nullable annotation present in the superclass.

Action: choose a consistent annotation based on the desired invariant.

Example:

````java
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

A consistent use of @Nullable on parameters across subtyping should prevent runtime issue like in:

````java
public class Main {

  String s;

  int foo() {
    A a = new B();
    return a.len(s);
  }
}
```

