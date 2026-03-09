/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

void set_both(int *p, int *q) {
  *p = 1;
  *q = 2;
}

void call_if_aliased(int *p, int *q) {
  if (p==q) set_both(p, q);
}

void always_call(int *p, int *q) {
  set_both(p, q);
}

void call_depending_on_alias(int *p, int *q) {
  call_if_aliased(p, q);
  always_call(p, q);
}

void test_aliased() {
  int x = 0;
  call_depending_on_alias(&x, &x);
}

void test_not_aliased() {
  int x = 0;
  int y = 1;
  call_depending_on_alias(&x, &y);
}
