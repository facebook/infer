/*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) 2017-present, Facebook, Inc.
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
