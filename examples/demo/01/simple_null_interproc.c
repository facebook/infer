/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

int* allocate_int();
void set(int* p, int value);

void deref_null() {
  int* p = allocate_int();
  set(p, 42);
}
