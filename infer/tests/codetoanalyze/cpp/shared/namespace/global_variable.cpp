/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace f1 {
int val;

struct A {
  static int v;
};
} // namespace f1

namespace f2 {
int val;
}

struct B {
  static int v;
};

struct C : public B {};

int div0_namepace_res() {
  f1::val = 1;
  f2::val = -2;
  return 1 / (f1::val + f2::val + 1);
}

int div0_static_field() {
  B::v = 1;
  f1::A::v = -2;
  return 1 / (f1::A::v + B::v + 1);
}

int div0_static_field_member_access(f1::A* a, C* b) {
  a->v = 1;
  b->v = -2;
  return 1 / (f1::A::v + B::v + 1);
}
