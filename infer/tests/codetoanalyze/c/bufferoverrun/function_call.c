/*
 * Copyright (c) 2016 - present
 *
 * Programming Research Laboratory (ROPAS)
 * Seoul National University, Korea
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdlib.h>

void arr_access(int* arr, char* p, int i) {
  int x = arr[0];
  arr[x] = 1; /* BUG */
  *(p + i) = 'a'; /* BUG */
}

void function_call() {
  int arr[10];
  arr[0] = 100;
  char* p = malloc(10);
  arr_access(arr, p, 20);
}

void ptr_set_to_zero(int* x) {
  *x = 0;
}

void call_by_ptr_good_FP() {
  int arr[10];
  int i = 99;
  ptr_set_to_zero(&i);
  arr[i] = 123;
}

void call_by_ptr_bad_FN() {
  int arr[10];
  int i = 5;
  ptr_set_to_zero(&i);
  arr[i - 1] = 123;
}
