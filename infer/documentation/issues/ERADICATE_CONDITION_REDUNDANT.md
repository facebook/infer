This report is inactive by default. Condition (x != null) or (x == null) when x
cannot be null: the first condition is always true and the second is always
false

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

Action: Make sure that the annotations are correct, as the condition is
considered redundant based on the existing annotations. In particular, check the
annotation of any input parameters and fields of the current method, as well as
the annotations of any method called directly by the current method, if
relevant. If the annotations are correct, you can remove the redundant case.
