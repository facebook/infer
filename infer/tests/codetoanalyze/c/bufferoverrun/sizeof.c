/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
  struct some_struct *x, *y;
  x = &(a[5]);
  y = &(a[4]);
  if (sizeof(struct some_struct) == x - y) {
    int a[0];
    a[1]; // report
  }
}

void sizeof_char_good_FP(int i) {
  char b[10];
  b[sizeof(b) - 1] = 123;
}
