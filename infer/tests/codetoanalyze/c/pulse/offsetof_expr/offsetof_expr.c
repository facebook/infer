/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stddef.h>

struct address {
  char v1[2];
  char v2[5];
  int v3;
};

int test_offsetof_expr() {
  int i = offsetof(struct address, v2);
  if (i == 2) {
    return 1 / 0;
  }
  return 42;
}

int test_offsetof_expr_nonlit() {
  size_t sum = 0;
  for (int i = 0; i < 2; i++) {
    sum += offsetof(struct address, v1[i]);
  }
  if (sum == 1) {
    return 1 / 0;
  }
  return 42;
}
