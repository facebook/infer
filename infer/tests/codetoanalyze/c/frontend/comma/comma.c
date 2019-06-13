/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
