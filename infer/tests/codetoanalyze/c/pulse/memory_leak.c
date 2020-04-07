/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

void malloc_no_free_bad() { int* p = malloc(sizeof(p)); }

int* malloc_returned_ok() {
  int* p = malloc(sizeof(p));
  return p;
}

void malloc_then_free_ok() {
  int* p = malloc(sizeof(p));
  if (p) {
    *p = 5;
    free(p);
  }
}

int* create_p() {
  int* p = malloc(sizeof(p));
  return p;
}

void malloc_interproc_no_free_bad() { int* p = create_p(); }

void malloc_interproc_no_free_bad2() {
  int* p = malloc(sizeof(p));
  int z = 3;
  int y = 4;
  int* q = p;
}
