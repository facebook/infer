/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
