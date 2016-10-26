/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
