/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <deque>

void iterate_over_deque_linear(std::deque<int> d) {
  for (auto it = d.begin(); it != d.end(); ++it) {
  }
}

void iteratec_over_deque_linear(std::deque<int> d) {
  for (auto it = d.cbegin(); it != d.cend(); ++it) {
  }
}

void range_based_loop_over_deque_linear(std::deque<int> d) {
  for (auto i : d) {
  }
}

void for_loop_over_deque_linear(std::deque<int> d) {
  for (int i = 0; i < d.size(); i++) {
  }
}
