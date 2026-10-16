/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <vector>

int deref_emplace_back_result_after_push_back_bad(std::vector<int>& vec) {
  int& elt = vec.emplace_back(7);
  vec.push_back(42);
  return elt;
}

int deref_emplace_back_no_args_result_after_push_back_bad(
    std::vector<int>& vec) {
  int& elt = vec.emplace_back();
  vec.push_back(42);
  return elt;
}

int deref_emplace_back_result_ok(std::vector<int>& vec) {
  int& elt = vec.emplace_back(7);
  return elt;
}

int reserve_then_emplace_back_result_ok(std::vector<int>& vec) {
  vec.reserve(vec.size() + 2);
  int& elt = vec.emplace_back(7);
  vec.emplace_back(8);
  return elt;
}

int emplace_back_results_are_distinct_ok(std::vector<int*>& vec, int x) {
  vec.reserve(vec.size() + 2);
  int*& first = vec.emplace_back();
  first = &x;
  int*& second = vec.emplace_back();
  second = nullptr;
  return *first;
}
