/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

void calloc_ok1() {
  int* arr = (int*)calloc(10, sizeof(int));
  arr[0] = 0;
  arr[9] = 0;
}

void calloc_bad1() {
  int* arr = (int*)calloc(10, sizeof(int));
  arr[-1] = 0;
  arr[10] = 0;
}
