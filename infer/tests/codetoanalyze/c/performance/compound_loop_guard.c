/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

/* while loop that contains && in the guard. It gives the correct bound  */
int compound_while(int m) {
  int i = 0;
  int j = 3 * i;
  while (j == 0 && i < m) {
    i++;
  }
  return j;
}

/* B will be in the loop and executed ~100 times-- we get infinity due to
 * control variable problem with gotos */
int simulated_while_with_and(int p) {
  int k = 0;
  int j = 0;
B:
  j++;
  if (k == 0 && j < 100) {
    goto B; // continue;
  }
  return k;
}

/* shortcut in the conditional, hence we won't loop, and get constant cost */
int simulated_while_shortcut(int p) {
  int k = 0;
  int j = 0;
B:
  j++;
  if (k == 1 && j < 100) {
    goto B; // continue;
  }
  return k;
}

/* p should be in control vars */
void while_and_or(int p) {
  int i = 0;
  while (p == 1 || (i < 30 && i > 0)) {
    i++;
  }
}

// should be constant cost, but due to p occuring in control vars, we would get
// +oo for p
int nested_while_and_or(int p) {
  int i = 0;
  int j = 3 * i;
  while (p == 1 || (i < 30 && i > 0)) {
    while (p == 1 || (j < 5 && j > 0)) {

      return j;
    }
    i++;
  }
  return j;
}
