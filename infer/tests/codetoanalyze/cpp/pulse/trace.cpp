/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

void make_alias(int** src, int** dst) { *dst = *src; }

void do_free(int* x) {
  int* y = x;
  int* z = y;
  free(y);
}

void trace_free_bad(int* x) {
  int* y;
  make_alias(&x, &y);
  do_free(y);
  int i = *x;
}
