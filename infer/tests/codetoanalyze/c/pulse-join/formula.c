/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// best run with --pulse-max-disjuncts 0 --pulse-over-approximate-reasoning

#include <stdlib.h>

void FP_same_facts_different_branches_ok(int* x) {
  int y;
  // pointer accesses to avoid the initial state pre-populating the abstract
  // values associated to the expression being compared
  if (random()) {
    if (*x == 42) {
      y = 1;
    } else {
      y = 0;
    }
  } else {
    if (*x == 42) {
      y = 1;
    } else {
      y = 0;
    }
  }
  if (*x == 42 && y == 0) {
    int* p = NULL;
    *p = 42;
  }
  if (*x != 42 && y == 1) {
    int* p = NULL;
    *p = 42;
  }
}
