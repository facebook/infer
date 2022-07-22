/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <vector>
#include <set>
#include <string>

struct Arr {
  int arr[2];
  std::vector<int> vec;
};

// The correct use of const reference when parameter is not modified
int read_first_ok(const Arr& a) { return a.vec[0]; }

// Non-const references are not reported on
std::string non_const_ref_ok(std::string& str) { return str + str; }

// R-value references are not reported on
void rvalue_ref_ok(Arr&& a) { auto& cpy = a; }

// vec is not modified, so should be const reference.
int read_first_bad(Arr a) { return a.vec[0]; }

int modify_first_ok(Arr a) {
  a.arr[0] += 8;
  return a.arr[0];
}

void ref_modified_ok(std::vector<int> vec) {
  auto& cpy = vec;
  cpy[0] += 8;
}

int get_first(const std::vector<int>& vec) { return vec[0]; }

void interprocedural_read_bad(std::vector<int> vec) {
  int first = get_first(vec);
}

// Sum vec by reading vector elements
int sum_in_loop_bad(std::vector<int> vec) {
  int total = 0;
  for (const int& x : vec)

    total += x; // vec is not modified here
  return total;
}
