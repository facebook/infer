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

void mutual1_bad();

void mutual3_bad() { mutual1_bad(); }

void mutual2_bad() { mutual3_bad(); }

void mutual1_bad() { mutual2_bad(); }

#include "recursion2.h"

void across_file_1() { across_file_2(); }
