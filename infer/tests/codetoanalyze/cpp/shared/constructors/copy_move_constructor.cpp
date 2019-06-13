/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace copy_move_constructor {

// NOTE: that test will break if we start doing copy elision
// will have default copy/move constructors - used to test
// whether they get translated correctly
struct X {
  int f;
};

// used to test whether backend can distinguish copy
// and move constructor
struct Y {
  int f;
  Y() = default;
  Y(const Y& y) = default;
  // move constructor with different behavior than copy constructor
  Y(const Y&& y) : f(y.f - 1) {}
};

X getX(int f) {
  X x;
  x.f = f;
  return x; // will call move constructor
}

Y getY(int f) {
  Y y;
  y.f = f;
  return y;
}

int copyX_div0() {
  X x1;
  x1.f = 0;
  X x2 = x1; // will call copy constructor
  return 1 / x2.f;
}

int moveX_div0() { return 1 / getX(0).f; }

int copyY_div0() {
  Y y1;
  y1.f = 0;
  Y y2 = y1; // will call copy constructor
  return 1 / y2.f;
}

int moveY_div0() { return 1 / getY(1).f; }

int moveY_moveY_copyY_div0() {
  Y y1 = getY(2); // move constructor in getY and in assignment
  Y y2 = y1;
  return 1 / y2.f;
}

int copyX_moveX_div1() {
  X x1;
  x1.f = 1;
  X x2 = x1;
  int d1 = 1 / x2.f;
  int d2 = 1 / getX(1).f;
  return d1 + d2;
}

int copyY_moveY_div1() {
  Y y1;
  y1.f = 1;
  Y y2 = y1;
  int d1 = 1 / y2.f;
  int d2 = 1 / getY(2).f;
  return d1 + d2;
}
} // namespace copy_move_constructor
