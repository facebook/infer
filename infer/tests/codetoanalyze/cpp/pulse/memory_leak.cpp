/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

void malloc_free_ok(int count) {
  int* e = (int*)malloc(count);
  int* res[] = {e};
  free(e);
}

void constant_index_ok(int count) {
  int* e = (int*)malloc(count);
  int* res[] = {e};
  free(res[0]);
}

void symbolic_index_ok(int x, int** res) {
  int* e = (int*)malloc(10);
  res[x] = e;
  free(res[x]);
}
