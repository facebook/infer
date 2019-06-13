/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdio.h>
#include <time.h>

void swap_bad(int* array, int i, int j) {
  int tmp = array[i];
  array[i] = array[j];
  array[j] = tmp;
}

void alias_mod_bad(int array[], int i, int j) {
  int* a = array;
  a[j] = i;
}

void fresh_arr_ok(int size) {
  int arr[size];
  for (int i = 0; i < size - 1; i++) {
    arr[i] = 0;
  }
}

void call_impure_with_local_ok(int size) {
  int arr[size];
  alias_mod_bad(arr, 0, 9);
}

void time_bad() {
  time_t rawtime;
  struct tm* timeinfo;

  time(&rawtime);
  timeinfo = localtime(&rawtime);
}
