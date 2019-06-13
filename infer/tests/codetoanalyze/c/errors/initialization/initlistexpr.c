/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int init_divide_by_zero() {
  int t[2][3][2] = {{{1, 1}, {2, 2}, {3, 3}}, {{4, 4}, {5, 5}, {1, 0}}};
  return t[0][1][0] / t[1][2][1];
}
