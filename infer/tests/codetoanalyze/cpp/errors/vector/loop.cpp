/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#include <vector>

void foreach_access_ok(std::vector<int>& vec) {
  if (vec.empty()) {
    // do nothing
  }
  for (const auto& elem : vec) {
    auto r = elem;
  }
}

void iterator_for_access_ok(std::vector<int>& vec) {
  if (vec.empty()) {
    // do nothing
  }
  for (auto it = vec.begin(); it != vec.end(); ++it) {
    auto r = *it;
  }
}

void empty_vector_loop_ok() {
  std::vector<int> vec;
  int* ptr = nullptr;
  for (const auto& elem : vec) {
    *ptr = elem; // this is unreachable
  }
}

void non_empty_vector_loop_bad(std::vector<int>& vec) {
  std::vector<int> x;
  int* ptr = nullptr;
  for (const auto& elem : vec) {
    *ptr = elem; // this is reachable
  }
}
