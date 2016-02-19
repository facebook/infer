/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

int comma_1() {
  int a = 9, b = 7;
  int d = (a = a * 2, b = 7 * a++);
  return d;
}

int comma_2() {
  int a = 9, b = 7;
  int d = (a = a * 2, b = 7 * a++, a + b + 9);
  return d;
}

int comma_3() {
  int a = 9, b = 7, c = 3;
  int d = (a = a * 2, b = 7 * a++, c = a + b + 9, c);
  return d;
}
