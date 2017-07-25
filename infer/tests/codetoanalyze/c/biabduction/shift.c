/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
int* return_depends_on_lshift(int x, int* p) {
  if (x < (1 << 7))
    return 0;
  else
    return p;
}

int return_nonnull_deref1_ok() {
  int y = 0;
  return *return_depends_on_lshift(1000, &y);
}

int return_null_deref1_bad() {
  int y = 0;
  return *return_depends_on_lshift(0, &y);
}

int* return_depends_on_rshift(int x, int* p) {
  if (x < (4 >> 2))
    return 0;
  else
    return p;
}

int return_nonnull_deref2_ok() {
  int y = 0;
  return *return_depends_on_rshift(2, &y);
}

int return_null_deref2_bad() {
  int y = 0;
  return *return_depends_on_rshift(0, &y);
}
