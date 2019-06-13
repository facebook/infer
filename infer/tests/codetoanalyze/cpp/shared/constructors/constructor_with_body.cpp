/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
} // namespace constructor_with_body
