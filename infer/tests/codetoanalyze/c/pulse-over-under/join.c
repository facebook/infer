/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// best run with --pulse-max-disjuncts 1 --pulse-over-approximate-reasoning

#include <stdlib.h>

void join_strong_update_ok() {
  int x;
  if (random()) {
    x = 0;
  } else {
    x = 0;
  }
  // could be an uninit FP if x's assignments disappear
  int y = x;
  if (y != 0) {
    // could be a null deref FP if x is not strongly updated after branching
    int* p = NULL;
    *p = 42;
  }
}
