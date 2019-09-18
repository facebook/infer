/*
 * Copyright (c) 2016-present, Programming Research Laboratory (ROPAS)
 *                             Seoul National University, Korea
 * Copyright (c) Facebook, Inc. and its affiliates.
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

void join_minmax_with_sum_unsigned_Good(unsigned int x, unsigned int y) {
  char a[x + y + 1];
  int i = 0;
  while (i < x + y) {
    if (i > 5) {
      y = 0;
    }
    i++;
  }
  a[i] = 2;
}

void call_join_minmax_with_sum_unsigned_Good() {
  join_minmax_with_sum_unsigned_Good(15, 50);
}

void join_minmax_with_sum_signed_Good_FP(int x, int y) {
  int s = x + y;
  if (s < 0)
    s = 0;
  char a[s + 1];
  int i = 0;
  while (i < x + y) {
    if (i > 5) {
      y = 0;
    }
    i++;
  }
  a[i] = 2;
}

void preciser_widen_Good(int x) {
  int idx = 0;
  int arr[10];
  while (idx < 10) {
    arr[idx] = 0;
    if (unknown()) {
      idx = x;
    }
  }
}
