/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>
void malloc_free_no_leak(int count) {
  int* e = (int*)malloc(count);
  int* res[] = {e};
  free(e);
}

void constant_index_no_leak_FP(int count) {
  int* e = (int*)malloc(count);
  int* res[] = {e};
  free(res[0]); // pulse creates a new abstract value each time so we
                // don't know that res[0] == e. Needs to be fixed.
}

void symbolic_index_no_leak(int x, int** res) {
  int* e = (int*)malloc(10);
  res[x] = e;
  free(res[x]); // it will know that res[x] == res[x], because the
                // abstract value involved in both array accesses will
                // be the same
}
