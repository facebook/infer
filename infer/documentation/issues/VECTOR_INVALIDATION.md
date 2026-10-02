An address pointing into a C++ `std::vector`, or into another container
of the standard library (see below), might have become invalid. This
can happen when an address is taken into a vector, then
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

The same applies to the buffer of a `std::basic_string`: the pointer
returned by `data()` or `c_str()`, an iterator returned by `begin()`,
or the pointer returned by `data()` of a `std::string_view` of the
string, might become invalid after a call to a mutating member
function such as `append()`, `clear()`, `push_back()`, `operator+=`,
`operator=` or `reserve()`, as the standard allows. Unlike for
`std::vector`, this is reported even after a call to `reserve()`.

```cpp
char deref_c_str_after_append_bad(std::string& s) {
  const char* p = s.c_str();
  s.append(1000, 'x'); // may re-allocate the buffer
  return *p;           // bad: p might be invalid
}
```

This issue is also reported for other containers of the standard library
(`std::deque`, `std::list`, `std::map`, `std::set`, `std::unordered_map`,
etc.) when an iterator or a reference to an element is used after an operation
that invalidates it, for instance after the element was removed with `erase()`
or `clear()`, or after an insertion into a `std::deque`, which invalidates all
its iterators. Dereferencing the `end()` iterator of a container is reported
too.

```cpp
void erase_in_loop_bad(std::map<int, int>& map) {
  for (auto it = map.begin(); it != map.end(); ++it) {
    if (it->second == 0) {
      map.erase(it); // bad: `it` is invalid after this, so `++it` is too
    }
  }
}
```
