/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
