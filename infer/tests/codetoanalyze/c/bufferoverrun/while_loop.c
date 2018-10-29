/*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void while_loop() {
  int i = 0;
  char* a = malloc(10);
  while (*(a + i) && i < 10) /* BUG */
    a[i++] = 1; /* SAFE */
}

struct S {
  struct S* f;
};

void dummy_func(struct S* x) {}

void diverge_on_narrowing() {
  struct S* x;
  while (1) {
    dummy_func(0);
    x = x->f;
  }
}
