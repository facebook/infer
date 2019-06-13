/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct A {
  int f;
  ~A() { f = 0; }
};

struct B {
  int f;
  ~B();
};

B::~B() { f = 1; }
