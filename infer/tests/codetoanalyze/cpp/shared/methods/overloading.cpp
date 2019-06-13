/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
