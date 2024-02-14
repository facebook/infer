This issue type indicates that a class with scope annotation A stores a field
with whose (dynamic) type (or one of its super types) is annotated with scope
B such that a scope nesting restriction is violated. By "stores", we mean
either directly or transitively.

A configuration is used to list the set of scopes and the must-not-hold relation.

In the following Java example, the set of scopes is Outer and Inner, and the must-not-hold
relation is simply \{(Outer, Inner)\}:
```java
@ScopeType(value = Outer.class)
class ClassOfOuterScope {
  final ClassOfInner c = new ClassOfInner(); // <-- warn here that ClassOfInner would leak.
}

@ScopeType(value = Inner.class)
class ClassOfInner {}
```

Here is a more detailed description of the analysis.

This analysis operates over Java bytecode. It assumes that types (classes, interfaces, enums,
etc.) may be annotated with so-called scope annotations. The analysis is parameterized by a set
of scopes and a "must-not-hold" relation over pairs of scopes, which it reads from a
configuration file.

The analysis aims to detect violations of the following property: if there exist a path of
fields from object OA to object OB and the type of OA (or one of its super-types) is annotated
with scope SA and the type of OB (or one of its super-types) is annotated with scope SB then
must-not-hold(SA, SB) must be false. Intuitively, the given objects have different scopes that
should not be nested, for example, different intended lifetimes, and a forbidden path from OA to
OB results in OB "leaking" out of the scope SA.

The implementation reads a configuration to determine a list of (scope) "generators" for each
type of scope and a scope class for each type of scope. A generator for a scope type SA is given
by the name of a class and a list of methods where it is understood that any of the methods
listed for the given class returns an object that is known to have scope SA. (This can be seen
as a form of lightweight modeling.) A scope class is the name of the class that represents a
given scope.
