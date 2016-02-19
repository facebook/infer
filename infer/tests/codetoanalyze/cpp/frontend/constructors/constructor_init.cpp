/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

struct A {
  int f;
  A(int f) : f(f) {}
};

struct B : public A {
  struct T {
    int v;
    T(int v) : v(v) {}
  };
  int f2;
  T t;
  B(int a) : A(a), f2(a), t(a) {}

  B(int a, int b) : B(a + b) { f2 = b; }
};

int f2_div0() {
  B b(0);
  return 1 / b.f2;
}

int f_div0() {
  B b(0);
  return 1 / b.f;
}

int t_div0() {
  B b(0);
  return 1 / b.t.v;
}

int delegate_constr_f_div0() {
  B b(-1, 1);
  int v = 1 / b.f2;
  return 1 / b.f;
}

int delegate_constr_f2_div0() {
  B b(-1, 0);
  int v = 1 / b.f;
  return 1 / b.f2;
}

int f_f2_div1() {
  B b(1);
  int v = 1 / b.f;
  int v2 = 1 / b.f2;
  int v3 = 1 / b.t.v;
  return v + v2;
}
