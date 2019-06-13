/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class A {
  int field;

 public:
  void init(int val) { field = val; }
  int method();
};

int A::method() {
  init(10); // call method on the object
  return field;
}

void test() {
  A* a_ptr;
  a_ptr->method();
}
