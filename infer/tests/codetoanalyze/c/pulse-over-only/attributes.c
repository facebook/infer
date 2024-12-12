/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// best run with --pulse-max-disjuncts 0 --pulse-over-approximate-reasoning

#include <stdlib.h>

int init_one_branch_only_bad() {
  int x;
  if (random()) {
    x = 42;
  }
  return x;
}

void FN_free_one_branch_only_bad() {
  int* p = (int*)malloc(sizeof(int));
  if (random()) {
    // FN because Invalid is used to represent freed locations and are kept by
    // joining
    free(p);
  }
}

// FN-ish: suppressed for now because we don't join histories properly
void FNsuppressed_invalid_one_branch_only_bad() {
  int* p = NULL;
  int x = 42;
  if (random()) {
    p = &x;
  }
  *p = 11;
}

int* get_maybe_invalid_pointer(int a) {
  if (a > 0) {
    int* p = (int*)malloc(sizeof(int));
    while (!p) {
    }
    return p;
  }
  return NULL;
}

void maybe_deref_pointer(int a, int* p) {
  if (a > 0) {
    *p = 42;
  }
}

void interproc_joined_invalid_deref_bad() {
  int* p = get_maybe_invalid_pointer(random());
  maybe_deref_pointer(random(), p);
}

// the price of over-approximation!
void FP_interproc_path_sensitive_valid_deref_ok() {
  int* p = get_maybe_invalid_pointer(10);
  maybe_deref_pointer(10, p);
}
