/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* note that the goal is flag the existence of recursive or mutually-recursive
   calls, not non-termination, so flagging a well-founded recursion is not
   considered a false positive */

void trivial_recursive_bad() { trivial_recursive_bad(); }

int factorial_bad(int x) {
  if (x > 0) {
    return x * factorial_bad(x - 1);
  } else {
    return 1;
  }
}

void FN_mutual1_bad();

void FN_mutual3_bad() { FN_mutual1_bad(); }

void FN_mutual2_bad() { FN_mutual3_bad(); }

void FN_mutual1_bad() { FN_mutual2_bad(); }

#include "recursion2.h"

void FN_across_file_1() { FN_across_file_2(); }
