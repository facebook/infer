This is reported when Infer detects an unnecessary copy of an object via `optional` value
construction where the source is not modified before it goes out of scope.  To avoid the copy, we
can move the source object or change the callee's type.

For example,

```cpp
void get_optional_value(std::optional<A> x) {}

void pass_non_optional_value(A x) {
  get_optional_value(x);
  // fix is to move as follows
  // get_optional_value(std::move(x));
}
```
