/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
/* t is also in control variables but once we have invariant analysis, it
 * shouldn't be */
int break_loop(int p, int t) {
  for (int i = 0; i < p; i++) {
    // do something
    if (t < 0)
      break;
    // do something
  }
  return 0;
}

/* t will be in control variables but once we have invariant analysis,
 * it shouldn't be. */
int break_loop_with_t(int p, int t) {
  for (int i = 0; i < p; i++) {
    // do something
    if (t < 0) {
      t++;
      break;
    }
    // do something
  }
  return 0;
}

/* calling break_loop with a negative t should give constant
   cost. Currently, this doesn't work since we can't do case analysis
   on the domain. */
int break_constant_FP(int p) { return break_loop(p, -1); }
