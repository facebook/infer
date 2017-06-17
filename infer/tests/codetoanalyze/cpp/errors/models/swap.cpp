/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
