/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <utility>

int swap_null_ok() {
  int a = 0;
  int *p = nullptr, *q = &a;
  std::swap(p, q);
  return *p;
}

int swap_null_bad() {
  int a = 0;
  int *p = nullptr, *q = &a;
  std::swap(p, q);
  return *q;
}
