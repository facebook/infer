/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include <stdnoreturn.h>

int* malloc_no_check_bad() {
  int* p = (int*)malloc(sizeof(int));
  *p = 42;
  return p;
}

void create_null_path_ok(int* p) {
  if (p) {
    *p = 32;
  }
}

void call_create_null_path_then_deref_unconditionally_ok(int* p) {
  create_null_path_ok(p);
  *p = 52;
}

void create_null_path2_ok(int* p) {
  int* q = NULL;
  if (p) {
    *p = 32;
  }
  // arguably bogus to check p above but not here, but the above could
  // also be macro-generated code so both reporting and not reporting
  // are sort of justifiable
  *p = 52;
}

// combine several of the difficulties above
void malloc_then_call_create_null_path_then_deref_unconditionally_ok(int* p) {
  int* x = (int*)malloc(sizeof(int));
  if (p) {
    *p = 32;
  }
  create_null_path_ok(p);
  *p = 52;
  free(x);
}

// pulse should remember the value of vec[64] because it was just written to
void nullptr_deref_young_bad(int* x) {
  int* vec[65] = {x, x, x, x, x, x, x, x, x, x, x, x, x, x,   x, x, x,
                  x, x, x, x, x, x, x, x, x, x, x, x, x, x,   x, x, x,
                  x, x, x, x, x, x, x, x, x, x, x, x, x, x,   x, x, x,
                  x, x, x, x, x, x, x, x, x, x, x, x, x, NULL};
  int p = *vec[64];
}

// due to the recency model of memory accesses, vec[0] can get forgotten
// by the time we have processed the last element of the
// initialization so we don't report here
void nullptr_deref_old_bad_FP(int* x) {
  int* vec[65] = {NULL, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
                  x,    x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
                  x,    x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x,
                  x,    x, x, x, x, x, x, x, x, x, x, x, x, x};
  int p = *vec[0];
}

void malloc_free_ok() {
  int* p = (int*)malloc(sizeof(int));
  free(p);
}

void wrap_free(void* p) { free(p); }

void interproc_free_ok() {
  int* p = (int*)malloc(sizeof(int));
  wrap_free(p);
}

noreturn void no_return();

void wrap_malloc(int** x) {
  *x = (int*)malloc(sizeof(int));
  if (!*x) {
    no_return();
  }
}

void call_no_return_good() {
  int* x = NULL;
  wrap_malloc(&x);
  *x = 5;
  free(x);
}
