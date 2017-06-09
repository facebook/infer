/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
int* static_local_ok() {
  static int x = 1;
  return &x;
}

int static_local_caller() {
  int* p = static_local_ok();
  return *p;
}
