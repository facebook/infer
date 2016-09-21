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
  // default parameters
  int fun_default(int a, int b = 10, int c = 20) { return a + b + c; }
};

void call_method_with_default_parameters() {
  A* a_ptr;
  a_ptr->fun_default(1, 2, 3);
  a_ptr->fun_default(1, 2);
  a_ptr->fun_default(1);
}
