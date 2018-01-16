/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
