/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

void l1_concrete_overrun_Bad() {
  int a[10];
  a[10] = 0;
}

void l1_concrete_underrun_Bad() {
  int a[10];
  a[-1] = 0;
}

void l1_symbolic_overrun_Bad(int i) {
  int a[10];
  if (i >= 10) {
    a[i] = 0;
  }
}

void l1_symbolic_underrun_Bad(int i) {
  int a[10];
  if (i < 0) {
    a[i] = 0;
  }
}

int zero_or_ten(int ten) {
  if (ten) {
    return 10;
  } else {
    return 0;
  }
}

void l2_concrete_overrun_Bad() {
  int a[10];
  a[zero_or_ten(1)] = 0;
}

void l2_concrete_underrun_Bad() {
  int a[9];
  a[zero_or_ten(0) - 1] = 0;
}

void l2_concrete_no_overrun_Good_FP() {
  int a[10];
  a[zero_or_ten(0)] = 0;
}

void l2_concrete_no_underrun_Good_FP() {
  int a[9];
  a[zero_or_ten(1) - 1] = 0;
}

void l2_symbolic_overrun_Bad(int n) {
  int a[n];
  a[n] = 0;
}

void l2_symbolic_no_overrun_Good(int n) {
  int a[n];
  if (n > 0) {
    a[n - 1] = 0;
  }
}

void l3_concrete_overrun_Bad() {
  int a[zero_or_ten(0) + 5];
  a[zero_or_ten(1)] = 0;
}

void l3_concrete_underrun_Bad() {
  int a[10];
  a[zero_or_ten(0) - 1] = 0;
}

void l3_concrete_no_overrun_Good_FP() {
  int a[zero_or_ten(1) + 5];
  a[zero_or_ten(1)] = 0;
}

void l3_concrete_no_underrun_Good_FP() {
  int a[10];
  a[zero_or_ten(1) - 1] = 0;
}

int less_than(int i, int n) { return i < n; }

void l4_widened_overrun_Bad() {
  int a[10];
  for (int i = 0; less_than(i, 11); i++) {
    a[i] = 0;
  }
}

void l4_widened_no_overrun_Good_FP() {
  int a[10];
  for (int i = 0; less_than(i, 10); i++) {
    a[i] = 0;
  }
}

int unknown_function();

void l5_external_Warn_Bad() {
  int a[10];
  a[unknown_function()] = 0;
}

int s2_symbolic_widened_Bad(int n) {
  int a[n];
  for (int i = n; less_than(i, 2 * n); i++) {
    a[i] = 0;
  }
}

int s2_symbolic_widened_Good_FP(int n) {
  int a[n];
  for (int i = n; less_than(i, n); i++) {
    a[i] = 0;
  }
}

void may_underrun_symbolic_Nowarn_Good(int n) {
  int a[n];
  a[n - 1] = 0;
}

void may_over_or_underrun_symbolic_Nowarn_Good(int n) {
  int a[10];
  a[n] = 0;
}

void may_over_or_underrun_symbolic2_Nowarn_Good(int n) {
  int a[n];
  a[1] = 0;
}
