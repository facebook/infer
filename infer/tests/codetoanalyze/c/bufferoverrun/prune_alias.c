/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
void prune_alias_le_Ok(int x) {
  int a[1];

  if (x <= x) {
    a[0] = 0;
  } else {
    a[1] = 0;
  }
}

void prune_alias_ge_Ok(int x) {
  int a[1];

  if (x >= x) {
    a[0] = 0;
  } else {
    a[1] = 0;
  }
}

void prune_alias_eq_Ok(int x) {
  int a[1];

  if (x == x) {
    a[0] = 0;
  } else {
    a[1] = 0;
  }
}

void prune_alias_lt_Ok(int x) {
  int a[1];

  if (x < x) {
    a[1] = 0;
  }
}

void prune_alias_gt_Ok(int x) {
  int a[1];

  if (x > x) {
    a[1] = 0;
  }
}

void prune_alias_ne_Ok(int x) {
  int a[1];

  if (x != x) {
    a[1] = 0;
  }
}

void prune_alias_not_Ok(int x) {
  int a[1];

  if (!(x == x)) {
    a[1] = 0;
  }

  if (!(x <= x)) {
    a[1] = 0;
  }

  if (!(x >= x)) {
    a[1] = 0;
  }
}

void prune_alias_and_Ok(int x) {
  int a[1];

  if (x == x && x != x) {
    a[1] = 0;
  }
}

void prune_alias_or_Ok(int x, int y) {
  int a[1];

  if (x != x || y != y) {
    a[1] = 0;
  }
}

void prune_alias_exp_Ok(int x) {
  int a[1];

  if (x + 1 != x + 1) {
    a[1] = 0;
  }
}

void prune_alias_exp2_CAF(int x) {
  int a[1];

  if (x + 1 != 1 + x) { // Condition always false
    a[1] = 0;
  }
}

void FP_prune_alias_exp_Ok(int* x) {
  int a[1];

  if (*x + 1 != 1 + *x) {
    a[1] = 0;
  }
}

#include <stdlib.h>

void prune_arrblk_ne_CAT(int* x) {
  int* y = x + 10;

  if (x != y) { // always true
    x[5] = 1;
  }
}

void call_prune_arrblk_ne_Ok() {
  int* x = (int*)malloc(sizeof(int) * 10);
  prune_arrblk_ne_CAT(x);
}

void call_prune_arrblk_ne_Bad() {
  int* x = (int*)malloc(sizeof(int) * 5);
  prune_arrblk_ne_CAT(x);
}

void prune_arrblk_eq_CAF(int* x) {
  int* y = x + 10;

  if (x == y) { // always false
    x[5] = 1; /* unreachable */
  }
}

void call_prune_arrblk_eq_Ok() {
  int* x = (int*)malloc(sizeof(int) * 5);
  prune_arrblk_eq_CAF(x);
}

void prune_minmax1_Ok(unsigned int x, unsigned int y) {
  if (x > 0) {
    if (y >= x + 1) {
      unsigned int z = y - 1;
    }
  }
}

void call_prune_minmax1_Ok() { prune_minmax1_Ok(0, 0); }

void prune_minmax2_Ok(unsigned int x, unsigned int y) {
  if (x > y) {
    unsigned int z = x - y;
  }
}

void call_prune_minmax2_Ok() { prune_minmax2_Ok(0, 2); }

void loop_prune_Good(int length, int j) {
  int i;
  char a[length];

  for (i = length - 1; i >= 0; i--) {
    if (j >= 0 && j < i) {
      /* here we always have i >= 1 */
      a[length - i] = 'Z';
    }
  }
}

void loop_prune2_Good_FP(int length, int j) {
  int i;
  char a[length];

  for (i = length - 1; i >= 0; i--) {
    /* need a relational domain */
    if (j < i && j >= 0) {
      /* here we always have i >= 1 */
      a[length - i] = 'Z';
    }
  }
}

void nested_loop_prune_Good(int length) {
  int i, j;
  char a[length];

  for (i = length - 1; i >= 0; i--) {
    for (j = 0; j < i; j++) {
      /* here we always have i >= 1 */
      a[length - i] = 'Z';
    }
  }
}

void bad_if_alias(int* x, int* y) {
  char a[1];
  if (x == y) {
    a[1] = 'A';
  }
}

void call_bad_if_alias_Bad_AlreadyReported() {
  int x;
  bad_if_alias(x, x);
}

void call_bad_if_alias_Good() {
  int x, y;
  bad_if_alias(x, y);
}

void bad_if_not_alias(int* x, int* y) {
  char a[1];
  if (x != y) {
    a[1] = 'B';
  }
}

void call_bad_if_not_alias_Good() {
  int x;
  bad_if_not_alias(x, x);
}

void call_bad_if_not_alias_Bad_AlreadyReported() {
  int x, y;
  bad_if_not_alias(x, y);
}

int unknown_function();

void latest_prune_join(int* a, int n) {
  int x;
  if (unknown_function()) {
    if (n < 4) {
      x = 1;
    } else {
      x = 0;
    }
  } else {
    if (n < 5) {
      x = 1;
    } else {
      return;
    }
  }
  if (x) {
    a[n] = 0;
  }
}

void call_latest_prune_join_1_Good() {
  int a[5];
  latest_prune_join(a, 3);
}

void call_latest_prune_join_2_Good() {
  int a[5];
  latest_prune_join(a, 10);
}

void call_latest_prune_join_3_Bad() {
  int a[2];
  latest_prune_join(a, 3);
}

void forget_locs_latest_prune(unsigned int n) {
  int x;
  int* a[5];
  if (n < 5) {
    x = 1;
  } else {
    x = 0;
    x = 2;
  }
  if (x) {
    a[n] = 0;
  }
}

void call_forget_locs_latest_prune_Bad() { forget_locs_latest_prune(10); }

void not_prune_multiple1_Bad() {
  int a[5];
  int m[2] = {0, 10};
  if (m[0] < 5) {
    a[m[1]] = 0;
  }
}

void not_prune_multiple2(int* m) {
  int a[5];
  if (m[0] < 5) {
    a[m[1]] = 0;
  }
}

void call_not_prune_multiple2_Good() {
  int m[2] = {0, 4};
  not_prune_multiple2(m);
}

void call_not_prune_multiple2_Bad_FN() {
  int m[2] = {0, 10};
  not_prune_multiple2(m);
}

void not_prune_multiple3_Bad() {
  int a[5];
  int* m = (int*)malloc(sizeof(int) * 2);
  m[0] = 0;
  m[1] = 10;
  if (*m < 5) {
    m++;
    a[*m] = 0;
  }
}

void not_prune_multiple4(int* m) {
  int a[5];
  if (*m < 5) {
    m++;
    a[*m] = 0;
  }
}

void call_not_prune_multiple4_Good() {
  int m[2] = {0, 4};
  not_prune_multiple4(m);
}

void call_not_prune_multiple4_Bad_FN() {
  int m[2] = {0, 10};
  not_prune_multiple4(m);
}

int* unknown1();
int* unknown2();

void unknown_alias_Good() {
  int* x = unknown1();
  int* y = unknown2();

  if (*x < *y) {
    int a[10]; // Here should be reachable.
    a[5] = 0;
  }
}

void unknown_alias_Bad() {
  int* x = unknown1();
  int* y = unknown2();

  if (*x < *y) {
    int a[10]; // Here should be reachable.
    a[10] = 0;
  }
}

void prune_linear_by_minmax(size_t s, size_t t) {
  if (s > 0 && s < 1000) {
    if (t >= s + 1) {
      size_t u = t - 2;
    }
  }
}

void call_prune_linear_by_minmax_Good() {
  prune_linear_by_minmax(unknown_function(), unknown_function());
}

void prune_int_by_pointer_Bad(int* p) {
  int* x = (int*)100;
  if (x == p) {
  }
  int a[5];
  a[5] = 0;
}
