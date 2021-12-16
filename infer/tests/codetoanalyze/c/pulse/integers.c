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

void float_div_bad() {
  float y = 5.0 / 2.0;
  if (y != 2.0) { // always true
    int* p = NULL;
    *p = 42;
  }
}

void float_comparison_latent(float f) {
  if (2 + f < 2.2) {
    int* p = NULL;
    *p = 42;
  }
}

// FN because x > y is translated as x >= y+1 by pulse, which is not
// valid for floats
void FN_call_float_comparison_bad() { float_comparison_latent(0.1); }
