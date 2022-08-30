This is reported when Infer detects an unnecessary copy into a field where
- the source is an rvalue-reference
- the source is not modified before it goes out of scope or is destroyed.

Note that the copy can be modified since it has the ownership of the object.

Fix: Rather than the copying into the field, the source should be moved into it.

For example,

```cpp
struct A {
  std::vector<int> vec;
};

class Test {
  A mem_a;

  void unnecessary_copy(A&& src) {
   mem_a = src;
   // fix is to move as follows
   // mem_a = std::move(src);
  }

};

```