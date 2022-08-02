This issue is reported when a function parameter is a) passed by value and b) is not modified inside the function. Instead, parameter can be passed by const reference, i.e. converted to a `const&` so that no unnecessary copy is created at the callsite of the function.

For example,

```cpp
#include <vector>

int read_first(const std::vector<int>& vec) { return vec[0]; }

void const_refable(std::vector<int> vec) {
  int first = read_first(vec); // vec is never modified, so the parameter should have type const&
}
```
