/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <array>
#include <iterator>

void loop_over_arr_size_constant(std::array<int, 5> arr) {
  for (int i = 0; i < arr.size(); i++) {
  }
}

void loop_over_arr_max_size_constant(std::array<int, 5> arr) {
  for (int i = 0; i != arr.max_size(); i++) {
    arr.at(i) = i * 10;
  }
}
