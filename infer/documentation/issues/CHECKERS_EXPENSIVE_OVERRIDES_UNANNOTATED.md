A method annotated with `@Expensive` overrides an un-annotated method.

Example:

```java
interface I {
  void foo();
}

class A implements I {
  @Expensive
  public void foo() {}
}
```
