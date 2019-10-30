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
bool equal(size_t x, size_t y) {
  return x == y;
}

void FP_unreachable_interproc_compare_ok(int *x, size_t y) {
  if (equal(y, 0)) {
    free(x);
  }
  if (equal_explicit(y,1)) {
    free(x);
  }
}

void compare_deref_ok(int *x) {
  if (!equal((size_t) x,0)) {
    *x = 42;
  }
}
