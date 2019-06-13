/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace struct_pass_by_value {

struct X {
  int f;
  X(int f) : f(f) {}
};

struct Y {
  Y(const X& x) : x(x) {}
  X x;
};

int get_f(X val) { return val.f; }

// val is passed by value, so it's not really a setter
void set_f(X val, int f) { val.f = f; }

int var_div0() {
  X x(0);
  return 1 / get_f(x);
}

int var_div1() {
  X x(1);
  return 1 / get_f(x);
}

int temp_div0() { return 1 / get_f(X(0)); }

int temp_div1() { return 1 / get_f(X(1)); }

int field_div0() {
  X x(0);
  Y y(x);
  return 1 / get_f(y.x);
}

int param_get_copied_div0() {
  X x(0);
  set_f(x, 1); // this won't change x
  return 1 / x.f;
}

int param_get_copied_div1() {
  X x(1);
  set_f(x, 0); // this won't change x
  return 1 / x.f;
}
} // namespace struct_pass_by_value
