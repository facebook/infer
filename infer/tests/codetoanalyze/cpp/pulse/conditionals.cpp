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

// pulse only tracks equality for now, not disequality
void FP_unreachable_ne_then_eq_ok(int* x, int y) {
  if (y != 0) {
    free(x);
  }
  if (y == 0) {
    free(x);
  }
}
