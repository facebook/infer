This issue is reported when a shared pointer parameter is a) passed by value and b) is used only for reading, rather than lifetime extension. At the callsite, this might cause a potentially expensive unnecessary copy of the shared pointer, especially when many number of threads are sharing it. To avoid this, consider 1) passing the raw pointer instead and 2) use `std::shared_ptr::get` at callsites.

For example,

```cpp
void callee(std::shared_ptr<T> x) {
  // read_T(*x);
}

void caller() {
  callee(shared_ptr);
}
```

can be changed to

```cpp
void callee(T* p) {
  // read_T(*p);
}

void caller() {
  callee(shared_ptr.get());
}
```
