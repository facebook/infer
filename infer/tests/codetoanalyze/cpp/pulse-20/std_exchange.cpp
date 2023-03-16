/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <utility>

void std_exchange_semantics_bad() {
  int a = 42;
  int b = 1;
  int c = std::exchange(a, a + b);
  if (a == 43 && c == 42) {
    int* p = nullptr;
    *p = 42;
  }
}

void std_exchange_semantics_ok() {
  int a = 42;
  int b = 1;
  int c = std::exchange(a, a + b);
  if (a != 43 || c != 42) {
    int* p = nullptr;
    *p = 42;
  }
}
