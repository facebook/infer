/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

class A {
 public:
  int fun(int a, int b);
  int fun(int a, int b, int c);
};

int A::fun(int a, int b, int c) { return a + b + c; }

int A::fun(int a, int b) { return a - b; }

void test() {
  A* a_ptr;
  // calling methods
  a_ptr->fun(1, 2);
  a_ptr->fun(1, 2, 3);
}
