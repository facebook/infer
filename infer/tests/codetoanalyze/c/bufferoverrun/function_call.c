/*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include <stdarg.h>

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

int ret_zero() { return 0; }

void call_function_ptr_good() {
  int (*func_ptr)(void) = &ret_zero;
  int arr[10];
  if ((*func_ptr)() != 0) {
    // unreacheable
    arr[10] = 1;
  }
}

void call_function_ptr_bad1() {
  int (*func_ptr)(void) = &ret_zero;
  int arr[10];
  if ((*func_ptr)() == 0) {
    arr[10] = 1;
  }
}

void access_index_1(int* arr) { arr[1] = 0; }
void access_index_4(int* arr) { arr[4] = 0; }

void call_access_index_1_on_local_array_Good() {
  int arr[4];
  access_index_1(arr);
}

void call_access_index_4_on_local_array_Bad() {
  int arr[4];
  access_index_4(arr);
}

void call_access_index_1_on_malloced_array_Good() {
  int* ptr = malloc(sizeof(int) * 4);
  access_index_1(ptr);
}

void call_access_index_4_on_malloced_array_Bad() {
  int* ptr = malloc(sizeof(int) * 4);
  access_index_4(ptr);
}

struct S2 {
  int arr[4];
};

void call_access_index_1_on_S2_Good(struct S2* s) { access_index_1(s->arr); }

void FN_call_access_index_4_on_S2_Bad(struct S2* s) { access_index_4(s->arr); }

struct S3 {
  int* ptr;
};

void call_access_index_1_on_S3(struct S3* s) { access_index_1(s->ptr); }

void call_access_index_4_on_S3(struct S3* s) { access_index_4(s->ptr); }

void call_call_access_index_1_on_S3_Good() {
  struct S3 s;
  s.ptr = malloc(sizeof(int) * 4);
  call_access_index_1_on_S3(&s);
}

void call_call_access_index_4_on_S3_Bad() {
  struct S3 s;
  s.ptr = malloc(sizeof(int) * 4);
  call_access_index_4_on_S3(&s);
}

void va_arg_int(int* a, ...) {
  va_list args;
  va_start(args, a);
  int i = va_arg(args, int);
  a[i] = 0;
  va_end(args);
}

void call_va_arg_int_Good_FP() {
  int a[10];
  va_arg_int(a, 5);
}

void call_va_arg_int_Bad() {
  int a[10];
  va_arg_int(a, 10);
}

struct S* id_S(struct S* r) {
  return r;
}

void call_id_S_Good_FP(struct S p) {
  int a[10];
  struct S* q = id_S(&p);
  if (q == 0) {
    a[10] = 0; // should be unreachable
  }
}

void call_id_S_Bad(struct S p) {
  int a[10];
  struct S* q = id_S(&p);
  if (q != 0) {
    a[10] = 0; // should be reachable
  }
}
