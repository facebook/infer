This report is inactive by default. Method m is annotated with @Nullable but the
method cannot return null

Example:

```java
class C {
  @Nullable String m() {
    String s = new String("abc");
    return s;
  }
}
```

Action: Make sure that the annotations are correct, as the return annotation is
considered redundant based on the existing annotations. In particular, check the
annotation of any input parameters and fields of the current method, as well as
the annotations of any method called directly by the current method, if
relevant. If the annotations are correct, you can remove the @Nullable
annotation.
