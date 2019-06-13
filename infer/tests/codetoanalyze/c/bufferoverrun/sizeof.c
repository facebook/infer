/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void eval_sizeof_bad() {
  if (sizeof(long long) < 10000) {
    // always true
    int a[0];
    a[1]; // report
  }
}

struct some_struct {
  int x0;
  int x1;
};

void static_stride_bad() {
  struct some_struct a[10];
  char *x, *y;
  x = (char*)&(a[5]);
  y = (char*)&(a[4]);
  if (sizeof(struct some_struct) == x - y) {
    int a[0];
    a[1]; // report
  }
}

void sizeof_char_good(int i) {
  char b[10];
  b[sizeof(b) - 1] = 123;
}
