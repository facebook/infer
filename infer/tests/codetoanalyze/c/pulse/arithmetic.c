/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>
#include <stdlib.h>

int return_non_negative() {
  int x = random();
  if (x < 0) {
    exit(1);
  }
  return x;
}

void return_non_negative_is_non_negative_ok() {
  if (return_non_negative() < 0) {
    int* p = NULL;
    *p = 42;
  }
}

void assume_non_negative(int x) {
  if (x < 0) {
    exit(1);
  }
}

void assume_non_negative_is_non_negative_ok() {
  int x = random();
  assume_non_negative(x);
  if (x < 0) {
    int* p = NULL;
    *p = 42;
  }
}

void if_negative_then_crash_latent(int x) {
  assume_non_negative(-x);
  int* p = NULL;
  *p = 42;
}

void FN_call_if_negative_then_crash_with_negative_bad(int x) {
  if (x < 0) {
    if_negative_then_crash_latent(x);
  }
}

void call_if_negative_then_crash_with_local_bad() {
  int x = random();
  if_negative_then_crash_latent(x);
}

float return_non_negative_float() {
  float x = ((float)random()) / (2 ^ 31 - 1);
  if (x < 0.) {
    exit(1);
  }
  return x;
}

void return_non_negative_float_is_non_negative_ok() {
  if (return_non_negative_float() < 0) {
    int* p = NULL;
    *p = 42;
  }
}

void assume_non_negative_float(float x) {
  if (x < 0.) {
    exit(1);
  }
}

void assume_non_negative_float_is_non_negative_ok() {
  float x = ((float)random()) / (2 ^ 31 - 1);
  assume_non_negative_float(x);
  if (x < 0.) {
    int* p = NULL;
    *p = 42;
  }
}

int less_than(int x, int y) { return x < y; }

void positive_then_compare_in_callee_ok() {
  int x = random();
  if (x <= 0) {
    return;
  }
  if (less_than(x, 0)) {
    return;
  }
  if (x == 0) {
    int* p = NULL;
    *p = 42;
  }
}

void positive_then_compare_in_callee_bad() {
  int x = random();
  if (x <= 0) {
    return;
  }
  if (less_than(x, 0)) {
    return;
  }
  if (x == 1) {
    int* p = NULL;
    *p = 42;
  }
}

void not_zero_or_one_then_compare_in_callee_ok() {
  int x = random();
  if (x < 0 || x == 0 || x == 1) {
    return;
  }
  if (less_than(x, 0)) {
    return;
  }
  if (x < 2) {
    int* p = NULL;
    *p = 42;
  }
}

void not_zero_or_one_then_compare_in_callee_bad() {
  int x = random();
  if (x < 0 || x == 0 || x == 1) {
    return;
  }
  if (less_than(x, 0)) {
    return;
  }
  if (x < 3) {
    int* p = NULL;
    *p = 42;
  }
}

void assume_at_least_three(int x) {
  if (x < 3) {
    exit(1);
  }
}

void assume_in_callee_then_compare_in_callee_ok() {
  int x = random();
  assume_at_least_three(x);
  if (less_than(x, 0)) {
    return;
  }
  if (x == 1) {
    int* p = NULL;
    *p = 42;
  }
}

void assume_in_callee_then_compare_in_callee_bad() {
  int x = random();
  assume_at_least_three(x);
  if (less_than(x, 0)) {
    return;
  }
  if (x == 3) {
    int* p = NULL;
    *p = 42;
  }
}

void negative_then_compare_in_callees_ok() {
  int x = random();
  int y = random();
  if (x >= -2) {
    return;
  }
  if (less_than(y, 0) || less_than(1, y)) {
    return;
  }
  if (y < x) {
    int* p = NULL;
    *p = 42;
  }
}

void negative_then_compare_in_callees_bad() {
  int x = random();
  int y = random();
  if (x >= -2) {
    return;
  }
  if (less_than(y, 0) || less_than(1, y)) {
    return;
  }
  if (x < y) {
    int* p = NULL;
    *p = 42;
  }
}

void FP_bounded_then_compare_in_callees_ok() {
  int x = random();
  int y = random();
  // FP: [7 <= x <= y <= 2] is infeasible but the callees' facts are only
  // recorded as linear equalities between non-negative variables, which are not
  // checked for feasibility together
  if (less_than(2, y) || less_than(y, x) || less_than(y, 1) ||
      less_than(x, 7)) {
    return;
  }
  int* p = NULL;
  *p = 42;
}
