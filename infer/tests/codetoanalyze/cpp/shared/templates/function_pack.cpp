/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// div(a,b,c,d) = 1/a + 1/b + 1/c + 1/d;
int div(int d) { return 1 / d; }
template <typename... Args>
int div(int v, Args... args) {
  return 1 / v + div(args...);
}

int div0_1arg() { return div(0); }

int div0_3args1() { return div(0, 2, 3); }
int div0_3args2() { return div(1, 0, 3); }
int div0_3args3() { return div(1, 2, 0); }
int div0_3args4() { return div(1, 0, 0); }
int div0_10args() { return div(1, 2, 3, 4, 5, 6, 7, 0, 9, 10); }

int no_div0_3_args() { return div(1, 2, 3); }
int no_div0_10args() { return div(1, 2, 3, 4, 5, 6, 7, 8, 9, 10); }
