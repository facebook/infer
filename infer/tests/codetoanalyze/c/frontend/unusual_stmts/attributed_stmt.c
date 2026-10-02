/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int callee(int n);

int musttail_return(int n) { __attribute__((musttail)) return callee(n); }

int gcc_unroll_loop(int n) {
  int sum = 0;
#pragma GCC unroll 4
  for (int i = 0; i < n; i++) {
    sum += i;
  }
  return sum;
}
