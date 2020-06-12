/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
/* Depending on whether p is positive, the program might loop forever */
int while_unique_def_FN(int p) {
  int j = 0;
  while (j < 10) {
    if (p > 0) {
      j = 500;
    } else {
      j = -1;
    }
  }
  return 0;
}

/* Expected O(n * m) */
void do_n_m_times_nested(int n, int m) {
  int k = n;
  int p = m;
  for (int i = 0; i < k; i++) {
    for (int j = 0; j < p; j++) {
    }
  }
}

/* Expected:O(p). Also inner loop will have t as invariant */
void two_loops_nested_invariant(int p) {
  int t = 0;
  int m = p;
  for (int i = 0; i < m; i++) {
    t = 3;
    for (int j = 0; j < t; j++) {
    }
  }
}

/* since the guard i is invariant, we will remove it from control
   variables, and hence the total cost will be constant, but the
   program diverges */
int while_infinite_FN() {
  int i = 0;
  while (i < 10) {
  }
  return 0;
}
