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

void sum_then_unreachable_npe_ok() {
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

void va_set_ptr(int n, ...) {
  va_list args;
  va_start(args, n);
  char** p = va_arg(args, char**);
  *p = (char*)malloc(4);
  va_end(args);
}

void va_arg_out_param_ok() {
  char* v;
  va_set_ptr(1, &v);
  free(v);
}

void va_arg_out_param_leak_bad() {
  char* v;
  va_set_ptr(1, &v);
}

void va_set_two_ptrs(int n, ...) {
  va_list args;
  va_start(args, n);
  char** p = va_arg(args, char**);
  char** q = va_arg(args, char**);
  *p = (char*)malloc(4);
  *q = (char*)malloc(4);
  va_end(args);
}

void va_arg_two_out_params_ok() {
  char *v, *u;
  va_set_two_ptrs(2, &v, &u);
  free(v);
  free(u);
}

// the first allocation is overwritten by the second one, which is reported in
// [va_set_two_ptrs] like for aliased non-variadic out-parameters
void va_arg_same_out_param_twice_bad() {
  char* v;
  va_set_two_ptrs(2, &v, &v);
  free(v);
}

void va_set_ptrs_loop(int n, ...) {
  va_list args;
  va_start(args, n);
  for (int i = 0; i < n; i++) {
    char** p = va_arg(args, char**);
    *p = (char*)malloc(4);
  }
  va_end(args);
}

void va_arg_out_param_loop_ok() {
  char* v;
  va_set_ptrs_loop(1, &v);
  free(v);
}

void* va_multi_malloc(int n, ...) {
  va_list args;
  va_start(args, n);
  char* start = (char*)malloc(4 * n);
  if (!start)
    return 0;
  char* res = start;
  for (int i = 0; i < n; i++) {
    char** p = va_arg(args, char**);
    *p = res;
    res += 4;
  }
  va_end(args);
  return start;
}

void va_multi_malloc_out_params_ok() {
  char *v, *u;
  if (!va_multi_malloc(2, &v, &u)) {
    return;
  }
  free(v);
}

// the out-parameters are not written when malloc fails
void va_multi_malloc_unchecked_bad() {
  char *v, *u;
  va_multi_malloc(2, &v, &u);
  free(v);
}

void va_set_ptr_with_copy(int n, ...) {
  va_list args, copy;
  va_start(args, n);
  va_copy(copy, args);
  char** p = va_arg(copy, char**);
  *p = (char*)malloc(4);
  va_end(copy);
  va_end(args);
}

void va_copy_out_param_ok() {
  char* v;
  va_set_ptr_with_copy(1, &v);
  free(v);
}

void va_set_ptr_after_variadic_call(int n, ...) {
  va_list args;
  va_start(args, n);
  char* tmp;
  va_set_ptr(1, &tmp);
  free(tmp);
  char** p = va_arg(args, char**);
  *p = (char*)malloc(4);
  va_end(args);
}

void va_arg_after_variadic_call_ok() {
  char* v;
  va_set_ptr_after_variadic_call(1, &v);
  free(v);
}

int va_read_int_ptr(int n, ...) {
  va_list args;
  va_start(args, n);
  int* p = va_arg(args, int*);
  int x = *p;
  va_end(args);
  return x;
}

void va_read_initialized_ok() {
  int x = 0;
  va_read_int_ptr(1, &x);
}

void va_read_uninitialized_bad() {
  int x;
  va_read_int_ptr(1, &x);
}

void va_set_int_ptr(int n, ...) {
  va_list args;
  va_start(args, n);
  int* p = va_arg(args, int*);
  *p = 42;
  va_end(args);
}

void va_write_null_bad() { va_set_int_ptr(1, NULL); }

// the position of the argument read from a [va_list] parameter is not known
void va_set_from_list(va_list args) {
  char** p = va_arg(args, char**);
  *p = (char*)malloc(4);
}

void va_set_ptr_with_list(int n, ...) {
  va_list args;
  va_start(args, n);
  va_set_from_list(args);
  va_end(args);
}

void FP_va_list_out_param_ok() {
  char* v;
  va_set_ptr_with_list(1, &v);
  free(v);
}
