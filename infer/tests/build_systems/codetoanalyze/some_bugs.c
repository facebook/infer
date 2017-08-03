/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
