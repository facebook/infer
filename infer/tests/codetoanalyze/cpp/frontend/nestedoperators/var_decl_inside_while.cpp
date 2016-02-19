/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

int simple_assignment() {
  int x = 10;
  int result = 0;
  while (int a = x) {
    result += a;
    x -= 1;
  }
  return 0;
}

int conditional_assignment() {
  int x = 10;
  int result = 0;
  while (int a = x > 0 ? x : 0) {
    result += a;
    x -= 1;
  }
  return 0;
}
