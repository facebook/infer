/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
void nop() { int k = 0; }

// Expected: Theta(m)
int two_loops_symb(int m) {
  int p = 10;

  for (int i = 0; i < m; i++) {
    nop();
  }
  for (int j = 0; j < m; j++) {
    nop();
  }
  return p;
}

// Expected: Theta(m + k)
int two_loops_symb_diff(int m, int k) {
  int p = 10;
  for (int i = 0; i < m; i++) {
    nop();
  }
  for (int j = 0; j < k; j++) {
    nop();
  }
  return p;
}
