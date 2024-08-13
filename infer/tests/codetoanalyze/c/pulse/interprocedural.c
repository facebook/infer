/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

void conditional_free(int x, int* y) {
  if (x > 5) {
    free(y);
  }
}

void conditional_free_then_use_latent(int x) {
  int* y = (int*)malloc(sizeof(int));
  conditional_free(x, y);
  if (y != NULL) {
    // arguably we should report here since the code is bad, but the current
    // heuristic classifies this as latent
    *y = 1;
  }
  // avoid memory leak
  if (x <= 5) {
    free(y);
  }
}

void call_conditional_free_then_use_bad() {
  conditional_free_then_use_latent(7);
}

// make sure this isn't classified as latent as callers have no control over the
// value of x being tested in the body of the function
void test_modified_value_then_error_bad(int* x) {
  *x = random();
  if (*x == 5) {
    int* p = NULL;
    *p = 42;
  }
}

// below is a test that the calling context appears in the correct order in the
// trace

void latent_dereference(int a, int* p) {
  if (a == 4) {
    *p = 42;
  }
}

void propagate_latent_1_latent(int a1) { latent_dereference(a1, NULL); }

void propagate_latent_2_latent(int a2) { propagate_latent_1_latent(a2); }

void propagate_latent_3_latent(int a3) { propagate_latent_2_latent(a3); }

void make_latent_manifest() { propagate_latent_3_latent(4); }

int* return_first(int* x, int a, int** out) {
  int* w = x;
  *out = w;
  return w;
}

int* return_null(int** out) {
  int* p = NULL;
  *out = p;
  return p;
}

// make sure the trace has all the details
void follow_value_by_ref_bad() {
  int* y;
  return_null(&y);
  int* z;
  return_first(y, 12, &z);
  *z = 42;
}

// make sure the trace has all the details
void follow_value_by_ret_bad() {
  int *dummy1, *dummy2;
  int* y = return_null(&dummy1);
  int* z = return_first(y, 12, &dummy2);
  *z = 42;
}

int* malloc_wrapper_1() {
  int* x;
  x = (int*)malloc(sizeof(int));
  return x;
}

int* malloc_wrapper_2(int b) {
  if (b) {
    return malloc_wrapper_1();
  }
  return NULL;
}

void free_wrapper(int* p, int b) {
  if (b) {
    free(p);
  }
}

void trace_correctly_through_wrappers_bad() {
  int* x = malloc_wrapper_2(1);
  // TODO: ideally we would trace that we didn't go into the free() branch of
  // the wrapper explicitly here to help understand the bug report
  free_wrapper(x, 0);
}
