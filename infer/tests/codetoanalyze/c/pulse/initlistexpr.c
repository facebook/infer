/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

int array_init_bad() {
  int t[2][3][2] = {{{1, 1}, {2, 2}, {3, 3}}, {{4, 4}, {5, 5}, {1, 0}}};
  if (t[0][1][0] == 2 && t[1][2][1] == 0) {
    int* p = NULL;
    *p = 42;
  }
}

int array_init_ok() {
  int t[2][3][2] = {{{1, 1}, {2, 2}, {3, 3}}, {{4, 4}, {5, 5}, {1, 0}}};
  if (t[0][1][0] != 2 || t[1][2][1] != 0) {
    int* p = NULL;
    *p = 42;
  }
}
