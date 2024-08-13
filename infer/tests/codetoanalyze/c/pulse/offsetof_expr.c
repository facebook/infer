/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stddef.h>
#include <stdlib.h>

struct address {
  char v1[2];
  char v2[5];
  int v3;
};

int test_offsetof_expr_bad() {
  int i = offsetof(struct address, v2);
  if (i == 2) {
    int* p = NULL;
    *p = 42;
  }
  return 42;
}

int FN_test_offsetof_expr_nonlit_bad() {
  size_t sum = 0;
  for (int i = 0; i < 2; i++) {
    sum += offsetof(struct address, v1[i]);
  }
  if (sum == 1) {
    int* p = NULL;
    *p = 42;
  }
  return 42;
}
