/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct X {
  int a;
  int b;
};

void test() {
  struct X x;
  x.a = 10;
  x.b = 20;
}
