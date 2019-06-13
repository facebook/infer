/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct X_struct {
  int a;
  int b;
};

class X_class {
 public:
  int a;
  int b;
};

void test() {
  // use pointers until c++ constructors are translated
  X_struct* xs;
  xs->a = 10;
  xs->b = 20;

  X_class* xc;
  xc->a = 10;
  xc->b = 20;
}
