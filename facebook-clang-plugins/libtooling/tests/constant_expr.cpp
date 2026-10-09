/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#pragma clang diagnostic ignored "-Wc++17-extensions"

enum E { A = 1 + 2 };

int f(int x) {
  if constexpr (sizeof(int) > 1) {
    x++;
  }
  switch (x) {
  case 2 * 2:
    return A;
  default:
    return 0;
  }
}
