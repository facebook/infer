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
