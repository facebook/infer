/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int exact_min_minus_min_linear_CAF(int x) {
  int size1[1];
  int y = 28;
  if (x < 5)
    y = x + 10;
  // y = [10 + min(18, x), 28]
  if (y > 28) {
    size1[-1] = 0;
  }
  return y;
}

int underapprox_min_minus_min_linear_CAF(int x) {
  int size1[1];
  int y = 10;
  if (x < 5)
    y = x + 20;
  // y = [20 + min(-10, x), 24]
  if (y > 24) {
    size1[-1] = 0;
  }
  return y;
}

void exact_min_plus_min_plus_min_UNDERRUN(int x, int b) {
  int size1[1];
  int y = exact_min_minus_min_linear_CAF(x);
  if (b)
    y = underapprox_min_minus_min_linear_CAF(x);
  y -= 29;
  // [-19 + min(0, x), -1]
  size1[y] = 0;
}
