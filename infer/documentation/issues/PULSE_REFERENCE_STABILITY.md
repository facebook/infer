The family of maps `folly::F14ValueMap`, `folly::F14VectorMap`, and by extension
`folly::F14FastMap` differs slightly from `std::unordered_map` as it does not
provide reference stability. Hence, when the map resizes such as when `reserve`
is called or new elements are added, all existing references become stale and
should not be used.

For example:

```cpp
#include <folly/container/F14Map.h>

void use_reference_after_growth_bad() {
  folly::F14FastMap<int, int> map = {{1, 1}, {2, 4}, {3, 9}};
  const auto& valueRef = map.at(1);
  map.emplace(4, 16);
  const auto valueCopy = valueRef;
}
```
