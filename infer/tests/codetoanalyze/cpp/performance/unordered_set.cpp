/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <unordered_set>
#include <iostream>
#include <string>

// worst case is quadratic
// https://www.cplusplus.com/reference/unordered_set/unordered_set/unordered_set/
// Expected: O(us1^2); got constant
void create_uset_range_quadratic_FN(std::unordered_set<std::string> us1) {
  std::unordered_set<std::string> us2(us1.begin(), us1.end());
}

// worst case is quadratic
// Expected: O(us1^2); got constant
void create_uset_copy_quadratic_FN(std::unordered_set<std::string> us1) {
  std::unordered_set<std::string> us2(us1);
}

// https://www.cplusplus.com/reference/unordered_set/unordered_set/operator=/
// Expected: O(s1); got constant
void copy_uset_linear_FN(std::unordered_set<std::string> us1,
                         std::unordered_set<std::string> us2) {
  us2 = us1;
}

void iterate_over_uset_linear(std::unordered_set<std::string> uset) {
  for (auto it = uset.begin(); it != uset.end(); ++it) {
  }
}

void iteratec_over_uset_linear(std::unordered_set<std::string> uset) {
  for (auto it = uset.cbegin(); it != uset.cend(); ++it) {
  }
}

void for_loop_over_uset_linear(std::unordered_set<std::string> uset) {
  for (int i = 0; i < uset.size(); i++) {
  }
}

void range_based_loop_over_uset_linear(std::unordered_set<std::string> uset) {
  for (auto i : uset) {
  }
}

// Expected: O(bucket(i).size); got constant
void iterate_over_bucket_linear_FN(std::unordered_set<std::string> uset,
                                   unsigned i) {
  for (auto it = uset.begin(i); it != uset.end(i); ++it) {
  }
}

// https://www.cplusplus.com/reference/unordered_set/unordered_set/bucket_count/
// Expected: O(uset.bucket_count() * bucket(i).size); got top
void loop_over_buckets_count_FP(std::unordered_set<std::string> uset) {
  for (unsigned i = 0; i < uset.bucket_count(); ++i) {
    iterate_over_bucket_linear_FN(uset, i);
  }
}

// erase is linear
// https://www.cplusplus.com/reference/unordered_set/unordered_set/erase/
// Expected: O(uset); got constant
void erase_from_uset_linear_FN(std::unordered_set<std::string> uset,
                               std::string str) {
  uset.erase(str);
}
