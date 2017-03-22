/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
namespace inheritance_casts {
struct A {
  int f;
};
struct B : public A {};

B getB(int f) {
  B x;
  x.f = f;
  return x;
}
A getA(int f) {
  A x;
  x.f = f;
  return x;
}

int div(const A& x) { return 1 / x.f; }

int div0_A() { return div(getA(0)); }

int div1_A() { return div(getA(1)); }

int div0_B() { return div(getB(0)); }

int div1_B() { return div(getB(1)); }
}
