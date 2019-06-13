/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
