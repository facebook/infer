/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

void malloc_no_free_bad() { // TODO implement the check
  int* p = malloc(sizeof(p));
}

int* malloc_returned_ok() {
  int* p = malloc(sizeof(p));
  return p;
}

void malloc_then_free_ok() {
  int* p = malloc(sizeof(p));
  *p = 5;
  free(p);
}
