/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct Point {
  double x, y;
};

void fun() {
  struct X {};
  X x;
}

Point blank = {3.0, 4.0};

struct A {
  static int a;
};

int A::a = 32;

struct B {
  static const int b = 52;
};
