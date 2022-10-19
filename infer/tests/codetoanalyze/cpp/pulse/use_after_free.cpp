/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

int use_after_free_simple_bad(int* x) {
  free(x);
  return *x;
}

void double_free_simple_bad(int* x) {
  free(x);
  free(x);
}

int* global_pointer;

void free_global_pointer_ok() { free(global_pointer); }

void double_free_global_bad() {
  free_global_pointer_ok();
  free_global_pointer_ok();
}

void free_null_then_deref_bad() {
  int* x = NULL;
  free(x);
  *x = 1;
}

void assumed_aliasing_latent(int* x, int* y) {
  if (x == y) {
    free(x);
    free(y);
  }
}

void trigger_assumed_aliasing_bad(int* x) { assumed_aliasing_latent(x, x); }

void assumed_aliasing2_latent(int* x, int* y) {
  if (x == y)
    ;
  free(x);
  free(y);
}

void trigger_assumed_aliasing2_bad(int* x) { assumed_aliasing2_latent(x, x); }

void assumed_aliasing3_latent(int* x, int* y) {
  free(x);
  if (x == y)
    ;
  free(y);
}

void trigger_assumed_aliasing3_bad(int* x) { assumed_aliasing3_latent(x, x); }

void FN_assumed_aliasing4_latent(int* x, int* y) {
  free(x);
  free(y);
  // we create the x==y case too late: x|->- * y|->- is already in the
  // state so adding x==y creates a contradition
  if (x == y)
    ;
}

void FN_trigger_assumed_aliasing4_bad(int* x) {
  FN_assumed_aliasing4_latent(x, x);
}
