/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

int sizeof_eval_ok(void) {
  int a = 4;
  int b = sizeof(a);
  char c[2];

  if (a % 4) { // 4 % 4 = 0
    int* p = NULL;
    *p = 42;
  }
  if (b % sizeof(a)) { // x % x = 0
    int* p = NULL;
    *p = 42;
  }
  if (sizeof(c) > 2) { // 2 > 2 is false
    int* p = NULL;
    *p = 42;
  }
  if ((sizeof(c) / sizeof(c[0])) != 2) { // (2 / 1) = 2
    int* p = NULL;
    *p = 42;
  }
  return 0;
}
