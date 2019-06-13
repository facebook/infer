/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void no_op() { int x = 0; }

// Expected: Theta(n)
void do_n_times(int n) {
  for (int i = 0; i < n; i++) {
    no_op();
  }
}

void do_2_times_Good() { do_n_times(2); }

// Expected: ~2000
void do_2K_times_Bad() { do_n_times(2000); }

// Expected: Theta(m^2)
void do_m2_times(int m) {
  for (int i = 0; i < m; i++) {
    do_n_times(m);
  }
}

// Expected: Theta(m^2)
void do_half_m2_times(int m) {
  for (int i = 0; i < m; i++) {
    do_n_times(i);
  }
}
