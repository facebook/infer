---
title: "Eradicate"
description: "The eradicate `@Nullable` checker for Java annotations."
---

The eradicate `@Nullable` checker for Java annotations.

Activate with `--eradicate`.

Supported languages:
- C/C++/ObjC: No
- Java: Yes
- C#/.Net: Yes

> "I call it my billion-dollar mistake. It was the invention of the null
> reference in 1965."
>
> [Tony Hoare](http://en.wikipedia.org/wiki/Tony_Hoare)

### What is Infer:Eradicate?

Infer:Eradicate is a type checker for `@Nullable` annotations for Java. It is part
of the Infer static analysis suite of tools. The goal is to eradicate null
pointer exceptions.

<a href="https://developer.android.com/reference/android/support/annotation/Nullable.html">@Nullable</a>
annotations denote that a parameter, field or the return value of a method can
be null. When decorating a parameter, this denotes that the parameter can
legitimately be null and the method will need to deal with it. When decorating a
method, this denotes the method might legitimately return null.

Starting from @Nullable-annotated programs, the checker performs a flow
sensitive analysis to propagate the nullability through assignments and calls,
and flags errors for unprotected accesses to nullable values or
inconsistent/missing annotations. It can also be used to add annotations to a
previously un-annotated program.

### What is the @Nullable convention?

If you say nothing, you're saying that the value cannot be null. This is the
recommended option when possible:

Program safely, annotate nothing!

When this cannot be done, add a @Nullable annotation before the type to indicate
that the value can be null.

### What is annotated?

Annotations are placed at the interface of method calls and field accesses:

- Parameters and return type of a method declaration.
- Field declarations.

Local variable declarations are not annotated: their nullability is inferred.

### How is Infer:Eradicate invoked?

Eradicate can be invoked by adding the option `--eradicate` to the checkers mode
as in this example:

```bash
infer run -a checkers --eradicate -- javac Test.java
```

The checker will report an error on the following program that accesses a
nullable value without null check:

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


## List of Issue Types

The following issue types are reported by this checker:
- [ERADICATE_ANNOTATION_GRAPH](/docs/1.1.0/all-issue-types#eradicate_annotation_graph)
- [ERADICATE_BAD_NESTED_CLASS_ANNOTATION](/docs/1.1.0/all-issue-types#eradicate_bad_nested_class_annotation)
- [ERADICATE_CONDITION_REDUNDANT](/docs/1.1.0/all-issue-types#eradicate_condition_redundant)
- [ERADICATE_FIELD_NOT_INITIALIZED](/docs/1.1.0/all-issue-types#eradicate_field_not_initialized)
- [ERADICATE_FIELD_NOT_NULLABLE](/docs/1.1.0/all-issue-types#eradicate_field_not_nullable)
- [ERADICATE_FIELD_OVER_ANNOTATED](/docs/1.1.0/all-issue-types#eradicate_field_over_annotated)
- [ERADICATE_INCONSISTENT_SUBCLASS_PARAMETER_ANNOTATION](/docs/1.1.0/all-issue-types#eradicate_inconsistent_subclass_parameter_annotation)
- [ERADICATE_INCONSISTENT_SUBCLASS_RETURN_ANNOTATION](/docs/1.1.0/all-issue-types#eradicate_inconsistent_subclass_return_annotation)
- [ERADICATE_META_CLASS_CAN_BE_NULLSAFE](/docs/1.1.0/all-issue-types#eradicate_meta_class_can_be_nullsafe)
- [ERADICATE_META_CLASS_IS_NULLSAFE](/docs/1.1.0/all-issue-types#eradicate_meta_class_is_nullsafe)
- [ERADICATE_META_CLASS_NEEDS_IMPROVEMENT](/docs/1.1.0/all-issue-types#eradicate_meta_class_needs_improvement)
- [ERADICATE_NULLABLE_DEREFERENCE](/docs/1.1.0/all-issue-types#eradicate_nullable_dereference)
- [ERADICATE_PARAMETER_NOT_NULLABLE](/docs/1.1.0/all-issue-types#eradicate_parameter_not_nullable)
- [ERADICATE_REDUNDANT_NESTED_CLASS_ANNOTATION](/docs/1.1.0/all-issue-types#eradicate_redundant_nested_class_annotation)
- [ERADICATE_RETURN_NOT_NULLABLE](/docs/1.1.0/all-issue-types#eradicate_return_not_nullable)
- [ERADICATE_RETURN_OVER_ANNOTATED](/docs/1.1.0/all-issue-types#eradicate_return_over_annotated)
- [ERADICATE_UNCHECKED_USAGE_IN_NULLSAFE](/docs/1.1.0/all-issue-types#eradicate_unchecked_usage_in_nullsafe)
- [ERADICATE_UNVETTED_THIRD_PARTY_IN_NULLSAFE](/docs/1.1.0/all-issue-types#eradicate_unvetted_third_party_in_nullsafe)
