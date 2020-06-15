A method annotated with `@PerformanceCritical` transitively calls a method annotated `@Expensive`.

Example:

```java
class C {
  @PerformanceCritical
  void perfCritical() {
    expensive();
  }

  @Expensive
  void expensive() {}
}
```
