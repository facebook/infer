An address pointing into a C++ `std::vector` might have become
invalid. This can happen when an address is taken into a vector, then
the vector is mutated in a way that might invalidate the address, for
example by adding elements to the vector, which might trigger a
re-allocation of the entire vector contents (thereby invalidating the
pointers into the previous location of the contents).

For example:

```cpp
void deref_vector_element_after_push_back_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  int* y = elt;
  vec.push_back(42); // if the array backing the vector was full already, this
                     // will re-allocate it and copy the previous contents
                     // into the new array, then delete the previous array
  std::cout << *y << "\n"; // bad: y might be invalid
}
```
