/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

struct Aggregate {
  int i;

  ~Aggregate() {}
};

void aggregate_reassign_ok() {
  const int len = 5;
  Aggregate arr[len];
  for (int i = 0; i < len; i++) {
    Aggregate s = {1};
    // assign with curly bracket syntax doesn't call constructor; need to
    // recognize that this is a reassignment anyway
    arr[0] = s; // shouldn't be flagged as a use-after-lifetime
  }
}

int multiple_invalidations_branch_bad(int n, int* ptr) {
  if (n == 7) {
    delete ptr;
  } else {
    delete ptr;
  }
  return *ptr;
}

int multiple_invalidations_loop_bad(int n, int* ptr) {
  for (int i = 0; i < n; i++) {
    if (i == 7) {
      delete ptr;
    } else {
      delete ptr;
    }
  }
  return *ptr;
}
