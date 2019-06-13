/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdbool.h>

int test2(int x) { return x; }

int test(int b) { return test2(b ? b : 1); }

int test1(int b) {
  int x = b ? b : 1;
  return x;
}

int test3(int b) {
  int x = b ?: 1;
  return x;
}

int test4(int b) { return test2(b ?: 1); }

int test5(int b) { return b ?: 1; }

int test6(int* p) {
  int z = 1 ? *p : 0;
  return z;
}

int test7(int b) { return test2(2 + test2(2)) ?: 2; }
