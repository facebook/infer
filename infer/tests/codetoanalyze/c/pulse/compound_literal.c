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

int return_zero() { return ((struct point){.y = 32, .x = 0}).x; }

int detect_zero_bad() {
  if (return_zero() == 0) {
    int* p = NULL;
    *p = 42;
  }
}

int detect_zero_ok() {
  if (return_zero() != 0) {
    int* p = NULL;
    *p = 42;
  }
}
