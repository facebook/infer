/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
void nop() { int k = 0; }

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
