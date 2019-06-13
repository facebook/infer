/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
