/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void two_accesses(int* arr) {
  if (arr[1] < 0) {
    arr[0] = 0;
  }
}

void one_alarm_is_enough() {
  int arr[1];
  two_accesses(arr);
}

void two_symbolic_accesses(int n) {
  int arr[1];
  arr[n] = 0;
  arr[n - 2] = 0; // Do not remove the associated condition
}

void tsa_one_alarm_Bad() { two_symbolic_accesses(3); }

void tsa_two_alarms_Bad() { two_symbolic_accesses(1); }
