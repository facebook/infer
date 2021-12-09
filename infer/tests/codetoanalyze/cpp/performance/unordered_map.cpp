/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <unordered_map>
#include <iostream>
#include <string>

void iterate_over_map_linear(std::unordered_map<std::string, std::string> m) {
  for (auto it = m.begin(); it != m.end(); ++it) {
  }
}

void iteratec_over_map_linear(std::unordered_map<std::string, std::string> m) {
  for (auto it = m.cbegin(); it != m.cend(); ++it) {
  }
}

void for_loop_over_map_linear(std::unordered_map<std::string, std::string> m) {
  for (int i = 0; i < m.size(); i++) {
  }
}

void range_based_loop_over_map_linear(
    std::unordered_map<std::string, std::string> m) {
  for (auto i : m) {
  }
}

// Expected: O(bucket(i).size); got top
void iterate_over_bucket_linear_FP(
    std::unordered_map<std::string, std::string> m, unsigned i) {
  for (auto it = m.begin(i); it != m.end(i); ++it) {
  }
}

// at is linear
// https://www.cplusplus.com/reference/unordered_map/unordered_map/at/
// Expected: O(m); got constant
void add_element_to_map_at_linear_FN(std::unordered_map<std::string, int> m,
                                     int n,
                                     std::string str) {
  m.at(str) = n;
}

// operator[] is linear
// https://www.cplusplus.com/reference/unordered_map/unordered_map/operator[]/
// Expected: O(m); got constant
void add_element_to_map_operator_linear_FN(
    std::unordered_map<std::string, int> m, int n, std::string str) {
  m[str] = n;
}

// insert is linear
// https://www.cplusplus.com/reference/unordered_map/unordered_map/insert/
// Expected: O(m); got constant
void add_element_to_map_insert_linear_FN(std::unordered_map<std::string, int> m,
                                         std::pair<std::string, int> el) {
  m.insert(el);
}

void erase_find_from_map_linear_FN(
    std::unordered_map<std::string, std::string> m, std::string str) {
  m.erase(m.find(str), m.end());
}
