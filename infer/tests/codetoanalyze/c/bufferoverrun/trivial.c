/*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void trivial_Bad() {
  int a[10];
  a[10] = 0; /* BUG */
}

void malloc_zero_Bad() {
  char* x;
  x = malloc(0);
  x = malloc(1);
}

int unknown_function();

void differentiate_array_info_Good() {
  int* p;
  if (unknown_function()) {
    p = (int*)malloc(sizeof(int) * 5);
  } else {
    p = (int*)malloc(sizeof(int) * 10);
    p = p + 5;
  }
  p[4] = 0;
}

void differentiate_array_info_Bad() {
  int* p;
  if (unknown_function()) {
    p = (int*)malloc(sizeof(int) * 5);
  } else {
    p = (int*)malloc(sizeof(int) * 10);
    p = p + 5;
  }
  p[5] = 0;
}
