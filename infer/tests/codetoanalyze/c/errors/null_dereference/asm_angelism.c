/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int test2() {
  int* x = 0;
  int y;
  int z;
  int h;
  asm("cpuid " : "=a"(x), "=b"(y), "=c"(z), "=d"(h) : "0"(0));
  return *x;
}
