/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int main() {
  auto x = new int[8];
  auto y = new int[8];
  y[0] = 42;
  auto x_ptr = x + 8; // one past the end
  if (x_ptr == &y[0]) // valid
    *x_ptr = 23;      // UB
  return y[0];
}
