/*
 * Copyright (c) 2016-present
 *
 * Programming Research Laboratory (ROPAS)
 * Seoul National University, Korea
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

struct S {
  int field;
};

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

void ptr_set_to_zero(int* x) { *x = 0; }

void struct_ptr_set_to_zero(struct S* s) { s->field = 0; }

void call_by_ptr_good() {
  int arr[10];
  int i = 99;
  ptr_set_to_zero(&i);
  arr[i] = 123;
}

void call_by_arr_good() {
  int arr[10];
  ptr_set_to_zero(&arr);
  arr[arr[0]] = 123;
}

void call_by_struct_ptr_good() {
  int arr[10];
  struct S* s = (struct S*)malloc(sizeof(struct S));
  s->field = 99;
  struct_ptr_set_to_zero(s);
  arr[s->field] = 123;
}

void call_by_ptr_bad() {
  int arr[10];
  int i = 5;
  ptr_set_to_zero(&i);
  arr[i - 1] = 123;
}

void call_by_arr_bad() {
  int arr[10];
  ptr_set_to_zero(&arr);
  arr[arr[0] - 1] = 123;
}

void call_by_struct_ptr_bad() {
  int arr[10];
  struct S* sp = (struct S*)malloc(sizeof(struct S));
  sp->field = 5;
  struct_ptr_set_to_zero(sp);
  arr[sp->field - 1] = 123;
}
