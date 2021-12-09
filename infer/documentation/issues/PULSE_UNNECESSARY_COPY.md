This is reported when Infer detects an unnecessary copy of an object via copy constructor where neither the source nor the copied variable are modified before the variable goes out of scope. Rather than the copy, a reference to the source object could be used to save memory.

For example,

```cpp
struct A {
  int a;
};

int unnecessary_copy(A& x){
  auto y = x; // calls copy constructor
  return y.a; // y is not modified after copy, hence we could avoid the copy by adding & after auto as below
}

int use_reference_instead(A& x){
  auto& y = x; // copy the ref only
  return y.a;
}
```