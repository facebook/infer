/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

void error_under_true_conditionals_bad(int* x) {
  if (1) {
    free(x);
  }
  if (2 == 2) {
    *x = 42;
  }
}

void simple_infeasible_error_path_ok(int* x) {
  free(x);
  if (0 == 1) {
    *x = 42;
  }

  int y = 0;
  if (y == 1) {
    *x = 42;
  }
  if (y) {
    *x = 42;
  }
  if (y != 0) {
    *x = 42;
  }
  if (!(y == 0)) {
    *x = 42;
  }
  if (!(!(y != 0))) {
    *x = 42;
  }
  if (!(!(!(0 == y)))) {
    *x = 42;
  }
}

void free_if(int* x, int b) {
  if (b) {
    free(x);
  }
}

void no_free_if_ok(int* x) {
  free_if(x, 0);
  *x = 42;
}

void free_if_deref_bad(int* x) {
  free_if(x, 1);
  *x = 42;
}

// Document some limitations, although there are so many for now that
// it's not really worth it. Add more tests when/if the analysis gets
// smarter than just constants.
void FP_infeasible_tricky_ok(int* x) {
  free_if(x, x == x);
  int y = 42;
  if (2 * y != y << 1) {
    free(x);
    *x = 42;
  }
}
