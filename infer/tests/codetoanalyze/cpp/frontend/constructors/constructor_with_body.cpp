/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

namespace constructor_with_body {

class X {
  int f;
  void init() { f = 0; }

 public:
  X() { init(); }

  X(int a, int b);

  int div() { return 1 / f; }
};

X::X(int a, int b) {
  int c = a + b;
  init();
  f = c;
}

void test_div0() {
  X x(-2, 2);
  x.div();
}

void test_div0_default_constructor() {
  X x;
  x.div();
}

void test_div1() {
  X x(0, 1);
  x.div();
}
}
