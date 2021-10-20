/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <map>
#include <string>
#include <iterator>

// operator[] is O(1) for arrays and O(log(n)) for maps.
// Expected: O(n * log(n)); got constant
void create_map_from_nlogn_linear_FN(int (&arr)[]) {
  std::map<int, int> m;
  int n = sizeof(*arr) / sizeof(arr[0]);
  for (int i = 0; i < n; i++)
    m[arr[i]]++;
}

// Expected: O(m); got constant
void loop_over_map_linear_FN(std::map<int, int> m) {
  for (auto i : m) {
  }
}

void loop_over_map_linear(std::map<int, int> m) {
  for (int i = 0; i < m.size(); i++) {
  }
}

// Expected: O(m); got constant
void loop_over_map_iterator_linear_FN(std::map<std::string, int> m) {
  std::map<std::string, int>::iterator it;
  for (it = m.begin(); it != m.end(); it++) {
  }
}

void loop_over_map_constant() {
  std::map<char, int> m;

  m['a'] = 101;
  m['b'] = 202;
  m['c'] = 302;

  for (auto i : m) {
  }
}

// map.at(c) is logarithmic
// Expected: O(log(m)); got constant
void insert_to_map_at_logm_FN(std::map<char, int> m, char c, int n) {
  m.at(c) = n;
}

// Expected: O(m); got constant
void loop_over_map_backwards_linear_FN(std::map<int, int> m) {
  for (auto it = m.crbegin(); it != m.crend(); ++it) {
  }
}

// m.count(e) is logarithmic
// Expected: O(log(m)); got constant
bool map_count_logm_FN(std::map<std::string, int> m, std::string str) {
  return m.count(str) > 0;
}

// emplace is logarithmic
// Expected: O(log(m)); got constant
void emplace_logm_FN(std::map<char, int> m, char c, int i) { m.emplace(c, i); }

// find is logarithmic
// Expected: O(log(m)); got constant
void find_logm_FN(std::map<char, int> m, char c) { m.find(c); }

// erase(val) is logarithmic
// Expected: m * log(m); got constant
void loop_and_erase_mlogm_FN(std::map<std::string, int> m) {
  std::map<std::string, int>::iterator it = m.begin();

  while (it != m.end()) {
    m.erase(it->first);
  }
}

// Expected: O(m1); got constant
void copy_assignment_map_linear_FN(std::map<char, int> m1,
                                   std::map<char, int> m2) {
  m2 = m1;
}

// Expected: O(m1); got constant
void move_assignment_map_linear_FN(std::map<char, int> m1,
                                   std::map<char, int> m2) {
  m2 = std::move(m1);
}

// Initializing a map from another map is O(n*log(n))
// (ref., https://www.cplusplus.com/reference/map/map/map/)
// Expected: m1 * log(m1); got constant
void init_map_mlogm_FN(std::map<char, int> m1) {
  std::map<char, int> m2(m1.begin(), m1.end());
}
