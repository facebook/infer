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
  static int fun(int);
};

int A::fun(int a) { return 1 / a; }

void div0_class() { A::fun(0); }

void div0_instance(A* a) {
  /* this will call static method as well */
  a->fun(0);
}
