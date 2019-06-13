/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace return_struct {

struct X {
  int f;
  // copy constructor that doesn't use init list
  X(const X& x) { f = x.f; }
  X() { f = 1; }
  int div() { return 1 / f; }
  int skip(); // this should be skip in the backend
};

X get(int a) {
  X x;
  x.f = a;
  return x;
}

int get_div0() {
  X x = get(0);
  return 1 / x.f;
}

int get_field_div0() {
  get(0).skip(); // this should do nothing - backend shouldn't crash
  return 1 / get(0).f;
}

int get_method_div0() { return get(0).div(); }

int get_div1() {
  X x = get(1);
  return 1 / x.f;
}

int get_field_div1() { return 1 / get(1).f; }

int get_method_div1() { return get(1).div(); }
} // namespace return_struct
