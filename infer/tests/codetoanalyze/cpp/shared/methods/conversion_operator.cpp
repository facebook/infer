/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

namespace conversion_operator {

struct X {
  operator int() { return f_; }
  operator bool() { return b_; }
  int f_;
  bool b_;
  X(int f, bool b) {
    f_ = f;
    b_ = b;
  }
  X(const X& x) {
    f_ = x.f_;
    b_ = x.b_;
  }
};

struct Y {
  // operator returning struct type
  operator X() { return X(f, b); }
  int f;
  int b;
};

int branch_div0() {
  X x(0, true);
  if (x) {
    int v = x;
    return 1 / v;
  }
  return x;
}

int y_branch_div0() {
  Y y;
  y.f = 0;
  y.b = true;
  if ((X)y) {
    int v = (X)y;
    return 1 / v;
  }
  return (X)y;
}

int branch_no_div() {
  X x(0, false);
  if (x) {
    int v = x;
    return 1 / v;
  }
  return x;
}

int branch_div1() {
  X x(1, true);
  if (x) {
    int v = x;
    return 1 / v;
  }
  return x;
}
}
