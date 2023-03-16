/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <type_traits>

namespace std_is_same_tests {
struct X;

template <typename T>
struct Y;

void simple_is_same_v_true_bad() {
  if (std::is_same_v<int, int>) {
    int* p = nullptr;
    *p = 42;
  }
}

void is_same_v_true_bad() {
  if (std::is_same_v<int, int> && std::is_same_v<X, X> &&
      !std::is_same_v<X, int> && !std::is_same_v<int, X> &&
      std::is_same_v<Y<X>, Y<X>> && !std::is_same_v<Y<X>, Y<int>> &&
      !std::is_same_v<Y<Y<X>>, Y<X>>) {
    int* p = nullptr;
    *p = 42;
  }
}

} // namespace std_is_same_tests
