/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/*WARNING: This is not being tested in the endtoend tests because it requires
 glog library.
 Only to be run manually */

#include <glog/logging.h>

int log_fatal_example() {
  int* p = nullptr;
  if (p == nullptr) {
    LOG(FATAL) << "this will crash\n";
  }
  return *p; // program should never get here
}

int check_example(int* a) {
  CHECK(a);
  return *a; // no null deref flagged by Infer
}

int check_not_null_example(int* p) {
  CHECK_NOTNULL(p);
  return *p;
}

int log_non_fatal_example() {
  int* a = nullptr;
  LOG_IF(INFO, a) << "well\n";
  LOG_IF(WARNING, a) << "well\n";
  LOG_IF(ERROR, a) << "well\n";
  return *a; // null deref flagged by Infer
}

int log_if_fatal_example() {
  int* a = nullptr;
  LOG_IF(FATAL, !a) << "well\n";
  return *a; // no null deref
}

int check_ne_example(int x) {
  CHECK_NE(x, 5);
  return x;
}

int check_eq_example(int x, int y) {
  CHECK_EQ(x, y);
  return x;
}

int check_le_example(int x) {
  CHECK_LE(x, 5);
  return x;
}

int check_gt_example(int x) {
  CHECK_GT(x, 5);
  return x;
}

int empty_check_ok(int index, std::vector<int> v) {
  CHECK(index >= 1 && index < v.size());
  return v[index]; // doesn't report empty vector
}
