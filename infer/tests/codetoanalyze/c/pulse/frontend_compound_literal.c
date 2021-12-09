/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

struct point {
  int x;
  int y;
};

void init_with_compound_literal_npe_bad() {
  struct point p = (struct point){32, 52};
  if (p.x == 32) {
    int* pointer = NULL;
    *pointer = 42;
  }
}

void init_with_compound_literal_npe_good() {
  struct point p = (struct point){32, 52};
  if (p.x == 1) {
    int* pointer = NULL;
    *pointer = 42;
  }
}
