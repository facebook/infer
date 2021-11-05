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

void even_cannot_be_odd_float_conv_FP() {
  int x = random();
  // two causes here: 1) Pulse represents floats as arbitrary values
  // and 2) the int conversion is omitted by the frontend
  if (x + x == (int)5.5) {
    int* p = NULL;
    *p = 42;
  }
}

void int_conversions_feasible_bad() {
  int x = random();
  int y = 3 / 2;
  int z = (int)1.234;
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
