/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
int null_test_deref_basic_bad(int* p) {
  int ret = *p;
  if (p == 0)
    return -1;
  return ret;
}

int null_test_deref_basic_ok() {
  int x = 0;
  int* p = &x;
  int ret = *p;
  if (p == 0)
    return -1;
  return ret;
}
