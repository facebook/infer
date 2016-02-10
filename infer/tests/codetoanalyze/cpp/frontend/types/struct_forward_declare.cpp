/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

// this will never be defined
struct Y;

struct X;
struct X {
  int f;
  int getF() { return f; }
  Y *y;
};

// forward declare again
struct X;

int X_div0() {
  X x;
  x.f = 0;
  return 1 / x.getF();
}

int X_ptr_div0(X *x) {
  x->f = 0;
  return 1 / x->getF();
}

int X_Y_div0() {
  X x;
  x.y = nullptr;
  x.f = 0;
  if (x.y) {
    return 1;
  }
  return 1 / x.getF();
}
