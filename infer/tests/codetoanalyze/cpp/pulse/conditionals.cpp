/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

void unreachable_eq_then_ne_ok(int* x, int y) {
  if (y == 0) {
    free(x);
  }
  if (y != 0) {
    free(x);
  }
}

void unreachable_ne_then_eq_ok(int* x, int y) {
  if (y != 0) {
    free(x);
  }
  if (y == 0) {
    free(x);
  }
}

bool equal_explicit(size_t x, size_t y) {
  if (x == y) {
    return true;
  }
  return false;
}

// need relational domain to give this a good spec
bool equal(size_t x, size_t y) { return x == y; }

void unreachable_interproc_compare_ok(int* x, size_t y) {
  if (equal(y, 0)) {
    free(x);
  }
  if (equal_explicit(y, 1)) {
    free(x);
  }
}

void compare_deref_ok(int* x) {
  if (!equal((size_t)x, 0)) {
    *x = 42;
  }
}

void arith_test_ok(int* x, int y, int z) {
  free(x);
  if (y != 0 && y != 1 && y >= 1) { // should infer y >= 2
    if (y < 2) { // always false
      *x = 42;
    }
  }
}

void add_test1_ok(int* x) {
  free(x);
  int y = 0;
  if (y + 1 != 1) { // always false
    *x = 42;
  }
}

void add_test2_ok(int* x, int y, int z) {
  free(x);
  if (y >= 0) {
    if (z >= 4 && z <= 42) {
      if (y + z < 4 || y + z <= 3 || z + 5 > 47) { // always false
        *x = 42;
      }
    }
  }
}

void add_test3_latent(int* x, int y, int z) {
  free(x);
  if (y > 2 && y + z > 5) { // sometimes true
    *x = 42;
  }
}

void add_test4_bad_FN(int* x) {
  free(x);
  // the concrete bound is never reached because it requires too many iterations
  // and we never widen
  for (int i = 0; i < 1000; i++) {
  }
  *x = 42;
}

void add_test5_latent(int* x, int n) {
  free(x);
  // the unknown bound is treated non-deterministically, good thing here
  for (int i = 0; i < n; i++) {
  }
  *x = 42;
}

void add_test6_bad_FNish(int* x, int n, int step) {
  free(x);
  // the loop should diverge but arguably the code is wrong and should
  // have a more explicit "false" condition (hence "FN-ish")
  for (int i = n - 1; i < n;) {
  }
  *x = 42;
}

void minus_test_ok(int* x) {
  free(x);
  if (-1 + 3 - 2 != 0) { // always false
    *x = 42;
  }
}

void binary_and_ok(int* x) {
  free(x);
  if (10 & 9) {
  } else {
    *x = 42;
  }
}

void binary_and_bad(int* x) {
  free(x);
  if (10 & 9) {
    *x = 42;
  }
}
