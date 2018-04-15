/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

// Tests that exercise precision of the analysis on control variables

// -- We computed infinity before for the following two tests--

// Loop's execution count doesn't depend on values of p,t,k
int loop_no_dep1(int k) {
  int p = 0;
  int t = 2 + k;
  for (int i = 0; i < 100; i++) {
    p++;
  }
  return p;
}

int foo(int i, int j) { return i + j; }

// Loop's execution count doesn't depend on values of p,t,k
int loop_no_dep2(int k) {
  int p = 0;
  int t = foo(p, k);
  for (int i = 0; i < 100; i++) {
    p++;
  }
  return p;
}

// -- Below examples should have worked, but due to the imprecision of CF
// analysis, they don't
//
// This example doesn't work since for the loop
// control vars={p,j} and since j in [-oo.+oo], we get oo count.
int if_bad(int j) {
  int p = 10;
  int i = 0;
  if (p < 10 + j) {
    p++;
  } else {
    p = j + 3;
    for (int k = 0; k < 100; k++) {
      j += 3;
    }
  }
  return p;
}

// We get +oo for this program, but if you take the first loop out,
// fake dependency disappears and we can get a proper bound
//
int two_loops() {
  int p = 10;
  int k = 3;
  int t = 2 + k;
  for (int j = 0; j < 6; j++) {
    k++;
  }
  for (int i = 0; i < 100; i++) {
    p = 3;
  }
  return p;
}

// We can't get program point A's execution count as 5 due to the weakness in CF
// analysis
int nested_loop() {
  int k = 0;
  for (int i = 0; i < 5; i++) {
  A:
    k = 0;
    for (int j = 0; j < 100; j++) {
      k = 3;
    }
  }
  return k;
}

// Unlike the above program, B will be inside the inner loop, hence executed
int simulated_nested_loop(int p) {
  int k = 0;
  int t = 5;
  int j = 0;
  for (int i = 0; i < 5; i++) {
  B:
    t = 3;
    j++;
    if (j < 100)
      goto B; // continue;
  }
  return k;
}
