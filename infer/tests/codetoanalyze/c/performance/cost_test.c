/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
// Cost: 5
int foo_OK() {
  int i, j;
  i = 17;
  j = 31;

  return i + j + 3 + 7;
}

// Cost: 17
int bar_OK() {

  int j = 0;

  j++;
  j++;
  j++;
  j = foo_OK();
  j++;

  return j;
}

// Cost: 25
int cond_OK(int i) {
  int x;

  if (i < 0) {
    x = bar_OK();
  } else {
    x = 1;
  }
  return x;
}

// Cost: 5
void alias_OK() {

  int i, j;

  j = i;
  i = ++i;
}

// Cost: 6
void alias2_OK() {

  int i, j, z;

  j = 1;
  z = 2;

  j = i;
  i = z;
}

// Cost: 1004
int loop0_bad() {

  for (int i = 0; i < 100; i++) {
    alias2_OK();
  }
  return 0;
}

// Cost: 1006
int loop1_bad() {

  int k = 100;
  for (int i = 0; i < k; i++) {
    alias2_OK();
  }
  return 0;
}

int FN_loop2(int k) {

  for (int i = 0; i < k; i++) {
    alias2_OK();
  }
  return 0;
}

// This is currently evaluated to Top as the analysis is not powerful enough
int FN_loop3(int k) {

  for (int i = k; i < k + 15; i++) {
    alias2_OK();
  }
  return 0;
}

// Cost: 218
// Shows that calling many times non expensive function can
// result in an expensive computation
int main_bad() {

  int k1, k2, k3, k4;

  cond_OK(2);
  k1 = bar_OK() + foo_OK() + cond_OK(15) * 2;
  k2 = bar_OK() + foo_OK() + cond_OK(17) * 3;
  k3 = bar_OK() + foo_OK() + cond_OK(11) * 3;
  k4 = bar_OK() + foo_OK() + cond_OK(19) * 3;
  return 0;
}
