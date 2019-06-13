/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace temp_object {

struct X {
  X(int a) { f = a; }
  X(int a, int b) { f = a; }
  // copy constructor without init list
  X(const X& x) { f = x.f; }
  int f;
  int div() { return 1 / f; }
};

int div(int f) { return 1 / f; }

// passing by value doesn't work
// int divX(X x) { return 1 / x.f; }

X getX(int a, int b) { return X(a, b); }

int assign_temp_div0() {
  X x = X(0, 1);
  return x.div();
}

int temp_field_div0() { return div(X(0, 1).f); }

int temp_field2_div0() { return div(X(0).f); }

int temp_method_div0() { return X(0, 1).div(); }

int getX_field_div0() { return div(getX(0, 1).f); }

int getX_method_div0() { return getX(0, 1).div(); }

int temp_field_div1() { return div(X(1, 0).f); }

int getX_field_div1() { return div(getX(1, 0).f); }
} // namespace temp_object
