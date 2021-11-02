/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <set>
#include <iterator>

void loop_over_set_size_linear(std::set<int> s) {
  for (int i = 0; i < s.size(); i++) {
  }
}

void loop_over_set_linear(std::set<int> s) {
  for (auto i : s) {
  }
}

void iterate_over_set_linear(std::set<int> s) {
  for (auto it = s.begin(); it != s.end(); it++) {
  }
}

void iteratec_over_set_linear(std::set<int> s) {
  for (auto it = s.cbegin(); it != s.cend(); it++) {
  }
}

void iterate_over_set_constant() {
  std::set<int> s;
  std::set<int>::iterator it;

  s.insert(40);
  s.insert(30);
  s.insert(60);

  for (it = s.begin(); it != s.end(); it++) {
  }
}

void iterate_over_set_linear(int n) {
  std::set<int> s;
  std::set<int>::iterator it;

  for (int i = 0; i < n; i++) {
    s.insert(i);
  }

  for (it = s.begin(); it != s.end(); it++) {
  }
}

// Expected: constant; got O(log(s))
void binary_search_over_set_constant_FP() {
  std::set<int> s;

  s.insert(40);
  s.insert(30);
  s.insert(60);

  binary_search(s.begin(), s.end(), 10);
}

void binary_search_over_set_logs(std::set<int> s) {
  binary_search(s.begin(), s.end(), 10);
}

// find is logarithmic
// Expected: O(log(s)); got constant
void loop_and_find_logs_FN(std::set<int> s, int n) {
  int i = 0;
  while (i < 10) {
    s.find(n);
    i++;
  }
}

void loop_over_set_size_constant(std::set<int> myset) {
  std::set<int> s1;

  s1.insert(40);
  s1.insert(30);
  s1.insert(60);

  int i = 0;
  while (i < s1.size()) {
  }
}

// equal_range(n) is logarithmic
// Expected: log(s); got constant
void equal_range_logs_FN(std::set<int> s, int n) {
  std::pair<std::set<int>::const_iterator, std::set<int>::const_iterator> ret;
  ret = s.equal_range(n);
}

// s.erase(val) is logarithmic
// Expected: s * log s; got constant
void while_not_empty_erase_val_slogs_FN(std::set<int> s) {
  while (!s.empty()) {
    s.erase(*s.begin());
  }
}

// Expected: O(s1); got constant
void copy_assignment_set_linear_FN(std::set<int> s1, std::set<int> s2) {
  s2 = s1;
}

// Expected: O(s1); got constant
void move_assignment_set_linear_FN(std::set<int> s1, std::set<int> s2) {
  s2 = std::move(s1);
}

// Initializing a set from an array is O(n*log(n))
// (ref., https://www.cplusplus.com/reference/set/set/set/)
// Expected: n * log(n); got constant
void init_set_from_list_nlogn_FN(int arr[], int n) {
  std::set<int> s1(arr, arr + n);
}

// s.count(i) is logarithmic
// Expected: n * log(s); got linear
void count_set_nlogs_FN(std::set<int> s, int n) {
  for (int i = 0; i < n; ++i) {
    if (s.count(i) != 0) {
    }
  }
}
