/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct X {
  int f;
};

struct A {
  X get(int p) {
    X x;
    return x;
  }
};

int test(A* a) {
  X x = a->get(1);
  return 1 / x.f;
}
