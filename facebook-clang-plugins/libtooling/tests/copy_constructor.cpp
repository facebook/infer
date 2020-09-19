/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct A {
  int a, b, c, d;
};

const A &f() {
  static A a;
  return a;
}

int g() {
  auto x = f();
  return x.a + x.b;
}
