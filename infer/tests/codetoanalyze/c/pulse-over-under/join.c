/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// best run with --pulse-max-disjuncts 1 --pulse-over-approximate-reasoning

#include <stdlib.h>

void FP_join_strong_update_ok() {
  int x;
  int z = 0;
  if (random()) {
    x = z;
  } else {
    x = z;
  }
  // could be an uninit FP if x's assignments disappear
  int y = x;
  if (y != 0) {
    // could be a null deref FP if x is not strongly updated after branching
    int* p = NULL;
    *p = 42;
  }
}
