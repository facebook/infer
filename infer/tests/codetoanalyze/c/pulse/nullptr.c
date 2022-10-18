/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>
#include <stdlib.h>
#include <stdnoreturn.h>

int* malloc_no_check_bad() {
  int* p = (int*)malloc(sizeof(int));
  *p = 42;
  return p;
}

void malloc_assert_ok() {
  int* p = (int*)malloc(sizeof(int));
  assert(p);
  *p = 42;
  free(p);
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

void create_null_path2_latent_FN(int* p) {
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
void malloc_then_call_create_null_path_then_deref_unconditionally_latent_FN(
    int* p) {
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
void FN_nullptr_deref_old_bad(int* x) {
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

void bug_after_malloc_result_test_bad(int* x) {
  x = (int*)malloc(sizeof(int));
  if (x) {
    int* y = NULL;
    *y = 42;
  }
}

void bug_after_abduction_bad(int* x) {
  *x = 42;
  int* y = NULL;
  *y = 42;
}

void bug_with_allocation_bad(int* x) {
  x = (int*)malloc(sizeof(int));
  int* y = NULL;
  *y = 42;
}

void null_alias_bad(int* x) {
  int* y = NULL;
  x = (int*)malloc(sizeof(int*));
  *x = 42;
}

void dereference(int* p) { int i = *p; }

void several_dereferences_ok(int* x, int* y, int* z) {
  int* p = x;
  *z = 52;
  dereference(y);
  *y = 42;
  *x = 32;
  *x = 777;
  *y = 888;
  *z = 999;
}

void report_correct_error_among_multiple_bad() {
  int* p = NULL;
  // the trace should complain about the first access inside the callee
  several_dereferences_ok(p, p, p);
}

int unknown(int x);

void unknown_is_functional_ok() {
  int* p = NULL;
  if (unknown(10) != unknown(10)) {
    *p = 42;
  }
}

void unknown_with_different_values_bad() {
  int* p = NULL;
  if (unknown(32) != unknown(52)) {
    *p = 42;
  }
}

void unknown_from_parameters_latent(int x) {
  int* p = NULL;
  if (unknown(x) == 999) {
    *p = 42;
  }
}

// is pruned away without the model
void random_non_functional_bad() {
  if (random() != random()) {
    int* p = NULL;
    *p = 42;
  }
}

void random_modelled_bad(int y) {
  int x = random();
  if (x == y) {
    int* p = NULL;
    *p = 42;
  }
}

void arithmetic_weakness_ok() {
  int x = random();
  int y = random();
  if (x < y && x > y) {
    int* p = NULL;
    *p = 42;
  }
}

int* unknown_int_pointer();

/* This report is currently suppressed because there is no evidence
that p can be NULL, but since it is compared to NULL *in the same
function* it may be worth reporting this. */
void FNsuppressed_no_invalidation_compare_to_NULL_bad() {
  int* p = unknown_int_pointer();
  int x;
  int* q = &x;
  if (p == NULL) {
    q = p;
  }
  *q = 42;
}

void incr_deref(int* x, int* y) {
  (*x)++;
  (*y)++;
}

void call_incr_deref_with_alias_bad(void) {
  int x = 0;
  int* ptr = &x;
  incr_deref(ptr, ptr);
  if (x == 2) {
    ptr = NULL;
  }
  x = *ptr;
}

void call_incr_deref_with_alias_good(void) {
  int x = 0;
  int* ptr = &x;
  incr_deref(ptr, ptr);
  if (x != 2) {
    ptr = NULL;
  }
  x = *ptr;
}
