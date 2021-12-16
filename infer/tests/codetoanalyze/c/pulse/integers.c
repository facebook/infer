/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void even_cannot_be_odd_local_ok(int y) {
  int x = y;
  if (x + x == 5) {
    int* p = NULL;
    *p = 42;
  }
}

void even_cannot_be_odd_parameter_ok(int x) {
  if (x + x == 5) {
    int* p = NULL;
    *p = 42;
  }
}

void even_cannot_be_odd_float_conv_ok() {
  int x = random();
  if (x + x == (int)5.5) {
    int* p = NULL;
    *p = 42;
  }
}

void FN_even_can_be_even_float_conv_bad() {
  int x = random();
  if (x + x == (int)6.5) {
    int* p = NULL;
    *p = 42;
  }
}

void FN_int_conversions_feasible_bad() {
  int x = random();
  int y = 3 / 2;
  int z = (int)1.234; // SIL ignores the cast so we get a contradiction here
  int w = x / 2;
  if (w == 5 && x == 10 && y == 1 && z == 1) {
    int* p = NULL;
    *p = 42;
  }
}

struct s {
  int i;
  int j;
};

void FPlatent_even_cannot_be_odd_fields_ok(struct s* x) {
  if (x->i + x->i == 5 || x->i + x->i + x->j + x->j == 5) {
    // latent issue is FP: arithmetic does not know 2x + 2y == 5 is impossible
    // when x and y are ints
    int* p = NULL;
    *p = 42;
  }
}

void FN_float_are_not_ints_bad() {
  float y = 5.0 / 2.0; // y is 2 here (incorrect)
  if (y != 2.0) { // This is always false
    int* p = NULL;
    *p = 42; // FN
  }
}
