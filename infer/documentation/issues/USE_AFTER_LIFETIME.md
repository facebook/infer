The lifetime of an object has ended but that object is being
accessed. For example, the address of a variable holding a C++ object
is accessed after the variable has gone out of scope:

```cpp
void foo() {
     X* p;
     { // new scope
       X x = X();
       p = &x;
     } // x has gone out of scope
     p->method(); // ERROR: you should not access *p after x has gone out of scope
}
```
