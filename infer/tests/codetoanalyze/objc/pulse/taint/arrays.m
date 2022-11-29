/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int int_source(void);
void sink_int(int);

void test_intra_proc_bad(void) {
  int arr[2];
  arr[0] = int_source();
  sink_int(arr[0]);
}

void test_intra_proc_good(void) {
  int arr[2];
  arr[0] = int_source();
  sink_int(arr[1]);
}

void test_intra_change_0_bad(int i) {
  int arr[2];
  arr[0] = int_source();
  if (i == 0)
    sink_int(arr[i]);
}

void test_intra_change_0_good(int i) {
  int arr[2];
  arr[0] = int_source();
  if (i == 0)
    sink_int(arr[1]);
}

void taint_cell(int* arr, int idx) { arr[idx] = int_source(); }

void sink_cell(int* arr, int idx) { sink_int(arr[idx]); }

void test_inter_proc_bad(void) {
  int arr[2];
  taint_cell(arr, 0);
  sink_cell(arr, 0);
}

void test_inter_proc_good(void) {
  int arr[2];
  taint_cell(arr, 0);
  sink_cell(arr, 1);
}
