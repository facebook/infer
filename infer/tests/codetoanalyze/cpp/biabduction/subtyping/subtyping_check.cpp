/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct A {
  int f;
  void setF() { f = 1; }
};

struct B : public A {
  int g;
  void setFG() {
    setF();
    g = 1;
    if (g == 1)
      g = 1 / 0;
    else
      g = 0;
  }
};
