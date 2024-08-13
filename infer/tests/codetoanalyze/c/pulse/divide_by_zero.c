/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// pulse doesn't detect division by zero yet
int FN_const_divide_by_zero_bad() {
  int x = 0;
  int y = 5;
  return y / x;
}
