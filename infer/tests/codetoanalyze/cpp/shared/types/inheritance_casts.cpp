/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
} // namespace inheritance_casts
