/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdio.h>
#include <stdlib.h>

int* set42(int* x) {
  *x = 42;
  return x;
}

void no_dangling_deref_ok() {
  int w, *z;
  z = set42(&w);
}

void no_dangling_deref1_ok() {
  int* y = malloc(sizeof(int));
  int* z;
  if (y) {
    z = set42(y);
    free(y);
  }
}

void dangling_deref_bad() {
  int* y;
  int* z;
  z = set42(y);
}

void intraproc_dangling_deref_bad() {
  int* y;
  int* z;
  *y = 42;
  z = y;
}

short union_ok(int* param) {
  union {
    int* a;
    short* b;
  } u;
  u.a = param;
  short* p = u.b;
  return *p;
}
