/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

extern int add_all_ints(int x, int y, int z, ...) __attribute__((sentinel));

void valid_call(int* a, int* b, int* c) {
  // fine
  int x = add_all_ints(0, 0, 0, a, b, c, NULL);
}

void truncated_call(void) {
  int a = 0, b = 1, c = 2, d = 3;
  int* p = NULL;

  // warning: p is NULL so only first argument sent to add_all_ints
  int x = add_all_ints(0, 0, 0, &a, p, &b, &c, &d, NULL);
}
