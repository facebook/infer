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

// var_arg semantics not taken into account
void FP_sum_then_unreachable_npe_ok() {
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

// #1937: memory allocated and stored through a variadic out-parameter used to be a
// false MEMORY_LEAK_C, because [va_arg]'s result was disconnected from the caller's
// argument. It is now connected via specialization, so the leak is no longer reported.
// A separate PULSE_UNINITIALIZED_VALUE false positive on the out-parameter remains and
// is marked with FP_ below.

void va_set_ptr(int n, ...) {
  va_list args;
  va_start(args, n);
  char** p = va_arg(args, char**);
  *p = (char*)malloc(4);
  va_end(args);
}

// no MEMORY_LEAK_C (fixed); FP_ marks the remaining UNINITIALIZED_VALUE on `v`
void FP_va_arg_out_param_no_leak_ok() {
  char* v;
  va_set_ptr(1, &v);
  free(v);
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

void FP_va_arg_two_out_params_no_leak_ok() {
  char *v, *u;
  va_set_two_ptrs(2, &v, &u);
  free(v);
  free(u);
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

void FP_va_arg_out_param_loop_no_leak_ok() {
  char* v;
  va_set_ptrs_loop(1, &v);
  free(v);
}

// Known limitation (#1937): a single [malloc] distributed across several
// out-parameters (pointers into one block) is not modelled, so both a spurious
// MEMORY_LEAK_C and UNINITIALIZED_VALUE are still reported.
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

void FP_va_multi_malloc_out_params_ok() {
  char *v, *u;
  va_multi_malloc(2, &v, &u);
  free(v);
}
