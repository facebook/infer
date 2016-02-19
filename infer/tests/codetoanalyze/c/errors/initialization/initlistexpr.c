/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

int init_divide_by_zero() {
  int t[2][3][2] = {{{1, 1}, {2, 2}, {3, 3}}, {{4, 4}, {5, 5}, {1, 0}}};
  return t[0][1][0] / t[1][2][1];
}
