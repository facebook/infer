/*
* Copyright (c) 2015 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*/

class X {
  int f;
  void init() { f = 0;}
public:
  X() {
    init();
    f = 3;
  }

  X(int a, int b);
};

X::X(int a, int b) {
  int c = a + b;
  init();
  f = c;
}
