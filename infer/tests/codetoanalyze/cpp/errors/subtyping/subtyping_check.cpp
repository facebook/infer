/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
