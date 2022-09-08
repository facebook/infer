This is similar to [PULSE_UNNECESSARY_COPY](#pulse_unnecessary_copy), but reported when a callee returns a copied value and it is not modified in its caller.  We may be able to return const-ref typed value or try `std::move` to avoid the copy.

For example,

```cpp
class MyClass {
  T v;
 public:
  T get() {
    return v; // v is copied here, which is avoidable.
  }
};

void caller(MyClass obj) {
  T x = obj.get();
  std::cout << x; // x is not modified.
}
```
