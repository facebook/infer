The return type of the overridden method is annotated @Nullable, but the
corresponding method in the superclass is not.

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

A consistent use of `@Nullable` on the return type across subtyping should prevent
runtime issue like in:

```java
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
