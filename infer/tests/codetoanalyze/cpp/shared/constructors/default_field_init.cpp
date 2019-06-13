/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct X {
  int a = -1;
  int b{-2};
  int c{};
  int d;
  X() = default;
  X(int a, int b) : a(a + b) {}
};

struct Y {
  X x1{1, 2};
  X x2{};
  X x3;
};

void test() { Y y; }
