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

// related to https://github.com/facebook/infer/issues/8
void FP_cleanup_malloc_good() {
  __attribute__((cleanup(cleanup_int))) int* x;
  x = malloc(sizeof(int));
  if (x != NULL) {
    *x = 10;
  }
  /* s goes out of scope. Cleanup function called - no leak */
}

// related to https://github.com/facebook/infer/issues/8
void FP_cleanup_string_good() {
  __attribute__((cleanup(cleanup_char))) char* s;
  s = strdup("demo string");
  /* s goes out of scope. Cleanup function called - no leak */
}
