/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void test1() {
  int* s1 = NULL;
  *s1 = 42;
}

void nullify(int* x) { *x = 0; }

void test2(int x) {
  int* s2 = NULL;
  nullify(s2);
}
