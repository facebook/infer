/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void do_two_times_Good(int n) {
  char a[1];
  for (int i = 0; i < n; i++) {
    n = 1;
    a[i] = 3;
  }
}

void do_two_times2_Good(int n) {
  char a[1];
  int k = n;
  for (int i = 0; i < k; i++) {
    k = 1;
    a[i] = 3;
  }
}

void call_do_two_times_Good_FP() { do_two_times_Good(5); }

void call_do_two_times2_Good_FP() { do_two_times2_Good(5); }
