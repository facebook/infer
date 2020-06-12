/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Tests that exercise precision of the analysis on control variables

// -- Below examples didn't work before, but enhancing CF analysis
// makes the analysis much more precise and we can get proper bounds
//
// This example works now because even though j in [-oo.+oo],
// since control vars={k} (notice that we will remove {p,j} in the else branch),
// we ignore j and find the right bound for the inner loop
int if_bad_constant(int j) {
  int p = 10;
  if (p < 10 + j) {
    p++;
  } else {
    p = j + 3;
    for (int k = 0; k < 10; k++) {
      j += 3;
    }
  }
  return p;
}

// Notice that removing {j,p} above doesn't create any problems if we are in a
// loop that depends on them. E.g.: below we still depend on {j} but in the
// conditional prune statement, we will remove the temp. var that map to inner
// {j}, not the outer {j}
int if_bad_loop_constant() {
  int p = 10;
  for (int j = 0; j < 5; j++) {
    if (j < 2) {
      p++;
    } else {
      p = 3;
      for (int k = 0; k < 10; k++) {
        int m = 0;
      }
    }
  }
  return p;
}

// The fake dependency btw first and second loop disappeared and we can get a
// proper bound
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

// We don't get a false dependency to m (hence p) since
// for if statements, we don't add prune variables as dependency
int loop_despite_inferbo_constant(int p) {

  int k = 100;
  for (int i = 0; i < k; i++) {
    int m = p + 3;
    if (m < 14) {
      p += 9;
    }
  }
  return p;
}

int nested_loop_constant() {
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
// around 105 times
int simulated_nested_loop_constant(int p) {
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

// B will be inside the inner loop and executed ~500 times
int simulated_nested_loop_more_expensive_constant(int p) {
  int k = 0;
  int t = 5;
  int j = 0;
  for (int i = 0; i < 5; i++) {
  B:
    t = 3;
    j++;
    if (j < 100)
      goto B; // continue;
    else {
      j = 0;
    }
  }
  return k;
}

int real_while_constant() {
  int i = 0;
  int j = 3 * i;
  while (i < 30) {
    j = j + i;
    i++;
  }
  return j;
}

// Examples with gotos

/* The following program is the version of real_while() with gotos */

int simulated_while_constant() {
  int i = 0;
  int j = 3 * i;
LOOP_COND:
  if (i < 30) {
    goto INCR;
  } else {
    goto RETURN;
  }
INCR:
  j = j + i;
  i++;
  goto LOOP_COND;
RETURN:
  return j;
}

/* Conditional inside goto loop  */
/* Expected: 5 * 100 */
int simulated_nested_loop_cond_in_goto_constant(int p) {
  int k = 0;
  int t = 5;
  int j = 0;
  for (int i = 0; i < 5; i++) {
  B:
    if (i > 2) {
      t = 3;
    } else {
      t = 4;
    }
    j++;
    if (j >= 100)
      j = 0;
    else {
      goto B; // continue;
    }
  }
  return k;
}
