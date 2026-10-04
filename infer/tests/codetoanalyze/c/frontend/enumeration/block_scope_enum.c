/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int block_scope_enum() {
  enum { kA, kB = 5, kC };
  int i = kA + kB + kC;
  enum E { kD = kC + 1 } e = kD;
  int helper(void), j = i + e;
  return j;
}
