/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

// we get two disjuncts one for each branch
void exit_positive(int a[10], int b) {
  if (b < 1) {
    exit(0);
  }
}

void unreachable_double_free_ok(int a[10], int b) {
  exit_positive(a, 0);
  free(a);
  free(a);
}

void store_exit(int* x, bool b) {
  if (b) {
    *x = 42;
    exit(0);
  }
}

void store_exit_null_bad() { store_exit(NULL, true); }
