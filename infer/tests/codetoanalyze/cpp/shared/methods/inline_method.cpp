/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class A {
 public:
  struct AIn {
    int fun() { return 1; }
  };
  AIn* in;
  // inline definition
  int fun() {
    int c = 10;
    return c + 1;
  }
};

void test_call() {
  A* a_ptr;
  a_ptr->fun();
  a_ptr->in->fun();
}
