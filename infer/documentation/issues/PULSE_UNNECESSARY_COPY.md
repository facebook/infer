Unnecessary copy of an object via copy constructor. This issue is raised in C when the copied object is not modified before the variable goes out of scope.

```cpp
struct A {
  int a;
};

 A& get_optional_ref() {
  static A a;
  return a;
}


Int foo(){
  B x;
  auto y = x; // calls copy constructor
  return y.a; // y is not modified after copy, hence we could avoid the copy by adding &
}

```