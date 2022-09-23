/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include <stdarg.h>

int sum(int n, ...) {
  va_list args;
  va_start(args, n);
  int sum = 0;
  for (int i = 0; i < n; i++) {
    sum += va_arg(args, int);
  }
  va_end(args);
  return sum;
}

void sum_one_then_npe_bad() {
  int one = sum(1, 1);
  int* p = NULL;
  *p = one;
}

// we run out of loop iterations before reaching 4
void FN_sum_four_then_npe_bad() {
  int four = sum(4, 1, 1, 1, 1);
  int* p = NULL;
  *p = four;
}

// we run out of loop iterations before reaching 4
void FN_sum_then_reachable_npe_bad() {
  int four = sum(4, 1, 1, 1, 1);
  if (four == 4) {
    int* p = NULL;
    *p = 42;
  }
}

// var_arg semantics not taken into account
void FP_sum_then_unreachable_npe_ok() {
  int one = sum(1, 1);
  if (one == 4) {
    int* p = NULL;
    *p = 42;
  }
}

int unknown_sum(int n, ...);

void unknown_sum_one_then_npe_bad() {
  int one = unknown_sum(1, 1);
  int* p = NULL;
  *p = one;
}

void unknown_sum_four_then_npe_bad() {
  int four = unknown_sum(4, 1, 1, 1, 1);
  int* p = NULL;
  *p = four;
}
