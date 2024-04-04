/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void intraprocedural_bad() {
  int* p = NULL;
  *p = 42;
}

int* return_null() { return NULL; }

void null_via_return_bad() {
  int* p = return_null();
  *p = 42;
}

void deref(int* x) { *x = 42; }

void via_deref_bad() {
  int* p = NULL;
  deref(p);
}

void null_via_return_via_deref_bad() {
  int* p = return_null();
  deref(p);
}

int* set_to_first(int* x, int** out) {
  int* w = x;
  *out = w;
  return w;
}

int* set_to_null(int** out) {
  int* p = NULL;
  *out = p;
  return p;
}

// make sure the trace has all the details
void follow_value_by_ref_bad() {
  int* y;
  set_to_null(&y);
  int* z;
  set_to_first(y, &z);
  *z = 42;
}

void latent(int a) {
  if (a == 4) {
    int* p = NULL;
    *p = 42;
  }
}

void propagate_latent_1_latent(int a1) { latent(a1); }

void propagate_latent_2_latent(int a2) { propagate_latent_1_latent(a2); }

void propagate_latent_3_latent(int a3) { propagate_latent_2_latent(a3); }

void make_latent_manifest() { propagate_latent_3_latent(4); }

void malloc_no_check_bad() {
  int* p = (int*)malloc(sizeof(int));
  *p = 42;
  free(p);
}

void deref_null_correct_null_assignment_bad() {
  int* p = NULL;
  int* q = NULL;
  *q = 42; // should pick the NULL from the line just above, not the one about p
}
