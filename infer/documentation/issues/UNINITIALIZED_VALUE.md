A value is read before it has been initialized. For example, in C:

```C
struct coordinates {
  int x;
  int y;
};

void foo() {
  struct coordinates c;
  c.x = 42;
  c.y++; // uninitialized value c.y!

  int z;
  if (z == 0) { // uninitialized value z!
    // something
  }
}
```
