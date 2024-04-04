/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <assert.h>
#include <stdlib.h>

void uninit_continues_execution_bad() {
  int x;
  int y = x + 1;
  int* p = NULL;
  *p = 42;
}

void leak_bad() {
  int* p = malloc(sizeof(int));
  assert(p);
}

void memleak_continues_execution_bad() {
  leak_bad();
  int* p = NULL;
  *p = 42;
}
