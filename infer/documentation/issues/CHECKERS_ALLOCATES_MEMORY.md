A method annotated with `@NoAllocation` transitively calls `new`.

Example:

```java
class C implements I {
  @NoAllocation
  void directlyAllocatingMethod() {
    new Object();
  }
}
```
