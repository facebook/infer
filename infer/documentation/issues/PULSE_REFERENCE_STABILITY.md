The family of maps `folly::F14ValueMap`, `folly::F14VectorMap`, and by extension
`folly::F14FastMap` differs slightly from `std::unordered_map` as it does not
provide reference stability. When the map resizes such as when `reserve` is
called or new elements are added, all existing references become invalid and
should not be used.

`operator[]` is an interesting case as it can easily introduce unsafe code when
used twice in the same expression. Depending on what keys are present and which
order the compiler sequences sub-expressions, an insert via `operator[]` can
invalidate a reference obtained in the same expression before it's read from.
Typically, those cases can be improved by using other map functions such as
`at`, `find`, `emplace`, or `insert_or_assign` to increase code quality and
safety.

Examples:

```cpp
#include <folly/container/F14Map.h>

void use_reference_after_growth_bad(folly::F14FastMap<int, int>& map) {
  const auto& valueRef = map.at(1);
  map.emplace(13, 71);
  const auto valueCopy = valueRef;
}

void unsafe_expressions_bad(folly::F14FastMap<int, int>& map) {
  // Unsafe expressions in situations where one or both keys are not present.
  map[13] = map[71];
  const auto p = map[13] * map[71];
  const auto q = f(map[13], map[71]);
}
```
