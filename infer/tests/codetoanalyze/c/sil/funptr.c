/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void apply(void (*f)(int*), int* p) { (*f)(p); }

void set_to_zero(int* p) { *p = 0; }

int call_funptr() {
  int x = 1;
  apply(&set_to_zero, &x);
  return x;
}
