/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#include <vector>

namespace iterator_compare {

bool is_empty(const std::vector<int>& vec) { return vec.begin() == vec.end(); }

bool not_empty(const std::vector<int>& vec) { return vec.begin() != vec.end(); }

void empty_no_deref1_ok() {
  int* p = nullptr;
  std::vector<int> vec;
  if (!is_empty(vec))
    *p = 42;
}

void empty_no_deref2_ok() {
  int* p = nullptr;
  std::vector<int> vec;
  if (not_empty(vec))
    *p = 42;
}

void empty_deref1_bad() {
  int* p = nullptr;
  std::vector<int> vec;
  if (is_empty(vec))
    *p = 42;
}

void empty_deref2_bad() {
  int* p = nullptr;
  std::vector<int> vec;
  if (!not_empty(vec))
    *p = 42;
}

void not_empty_no_deref1_ok() {
  int* p = nullptr;
  std::vector<int> vec = {1, 2, 3, 4};
  if (is_empty(vec))
    *p = 42;
}

void not_empty_no_deref2_ok() {
  int* p = nullptr;
  std::vector<int> vec = {1, 2, 3, 4};
  if (!not_empty(vec))
    *p = 42;
}

void not_empty_deref1_bad() {
  int* p = nullptr;
  std::vector<int> vec = {1, 2, 3, 4};
  if (!is_empty(vec))
    *p = 42;
}

void not_empty_deref2_bad() {
  int* p = nullptr;
  std::vector<int> vec = {1, 2, 3, 4};
  if (not_empty(vec))
    *p = 42;
}

} // namespace iterator_compare
