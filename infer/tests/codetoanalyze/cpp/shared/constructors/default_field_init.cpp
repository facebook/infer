/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
