/*
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <array>

int std_array_bo_Bad() {
  std::array<int, 42> a;
  return a[42];
}

int normal_array_bo() {
  int b[42];
  return b[42];
}
