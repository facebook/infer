/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>
#include <string.h>

void cleanup_char(char** x) { free(*x); }

void cleanup_int(int** x) { free(*x); }

void no_cleanup(int** x) { /* nothing */ }

void cleanup_malloc_ok() {
  __attribute__((cleanup(cleanup_int))) int* x;
  // attribute cleanup is done on *x not x, resulting in NPE, Uninit, and
  // memleak FPs
  x = malloc(sizeof(int));
  if (x != NULL) {
    *x = 10;
  }
  /* x goes out of scope. Cleanup function called - no leak */
}

void FN_wrong_cleanup_malloc_bad() {
  // no_cleanup does nothing, hence there is a leak
  __attribute__((cleanup(no_cleanup))) int* x;
  x = malloc(sizeof(int));
}

// related to https://github.com/facebook/infer/issues/8
void cleanup_string_ok() {
  __attribute__((cleanup(cleanup_char))) char* s;
  s = strdup("demo string");
  /* s goes out of scope. Cleanup function called - no leak */
}
