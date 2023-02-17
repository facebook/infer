/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <optional>
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
} // namespace unnecessary_copy
