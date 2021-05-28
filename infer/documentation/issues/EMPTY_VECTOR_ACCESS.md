This error type is reported only in C++, in versions >= C++11.

The code is trying to access an element of a vector that Infer believes to be
empty. Such an access will cause undefined behavior at runtime.

```cpp
#include <vector>
int foo(){
  const std::vector<int> vec;
  return vec[0]; // Empty vector access reported here
}
```
