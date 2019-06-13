/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <algorithm>

struct X {
  X(int f) : f(f) {}
  int f;
};
bool operator<(const X& x1, const X& x2) { return x1.f < x2.f; }

struct X_inv {
  X_inv(int f) : f(f) {}
  int f;
};
bool operator<(const X_inv& x1, const X_inv& x2) { return x1.f > x2.f; }

int min_int_div0() { return 1 / std::min(1, 0); }

int min_int_div_1() { return 1 / std::min(-1, 0); }

int max_int_div1() { return 1 / std::max(1, 0); }

int max_int_div0() { return 1 / std::max(0, -1); }

int min_X_div0() {
  X x0(0), x1(1);
  return 1 / std::min(x0, x1).f;
}

int max_X_div1() {
  X x0(0), x1(1);
  return 1 / std::max(x0, x1).f;
}

int min_X_inv_div1() {
  X_inv x0(0), x1(1);
  return 1 / std::min(x0, x1).f;
}

int max_X_inv_div0() {
  X_inv x0(0), x1(1);
  return 1 / std::max(x0, x1).f;
}
