/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

class X {
  int f;

 public:
  X(int a, int b = 0);
  int div() { return 1 / f; }
};

X::X(int a, int b) { f = a + b; }

void test() {
  X x1(0);
  X x2(1);
  X x3(0, 1);
}
