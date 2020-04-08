/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

int x;

void exit_positive_impure_FN(int a[10], int b) {
  if (b > 0) {
    exit(0);
  }
}

void unreachable_impure_FN(int a[10], int b) {
  exit_positive_impure_FN(a, 10);
  x = 9;
}
