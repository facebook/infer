/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstdlib>
#include <new>

int malloc_unchecked_ok() {
  int* p = (int*)malloc(sizeof(int));
  *p = 1;
  int r = *p;
  free(p);
  return r;
}

int nothrow_new_unchecked_ok() {
  int* p = new (std::nothrow) int(1);
  int r = *p;
  delete p;
  return r;
}

void nothrow_new_leak_bad() {
  int* p = new (std::nothrow) int(1);
  *p = 2;
}
