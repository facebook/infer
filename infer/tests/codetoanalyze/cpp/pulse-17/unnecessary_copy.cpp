/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <optional>
#include <string>
#include <vector>

namespace unnecessary_copy {
struct Arr {
  int arr[2];
  std::vector<int> vec;
};

void get_optional_const_ref(const std::optional<Arr>& x) {}

void call_get_optional_const_ref_bad(Arr x) {
  x.arr[0] = 42;
  get_optional_const_ref(x);
}

void call_get_optional_const_ref_ok(Arr x) {
  x.arr[0] = 42;
  get_optional_const_ref(std::move(x));
}

void get_optional_value(std::optional<Arr> x) {}

void call_get_optional_value_bad(Arr x) {
  x.arr[0] = 42;
  get_optional_value(x);
}

void call_get_optional_value_ok(Arr x) {
  x.arr[0] = 42;
  get_optional_value(std::move(x));
}

void call_get_optional_value_static_cast_ok(Arr x) {
  x.arr[0] = 42;
  get_optional_value(static_cast<Arr&&>(x));
}

class OptionalFieldOk {
 public:
  template <typename T>
  OptionalFieldOk(T&& x) : x(std::forward<T>(x)) {}

 private:
  std::optional<Arr> x;
};

void construct_optional_field(Arr x) { OptionalFieldOk o(std::move(x)); }

void call_std_distance_ok(const std::string& s) {
  std::string t = s;
  t.data()[0] = 'a'; // t is modified
  std::distance(t.begin(), t.end());
}
} // namespace unnecessary_copy
