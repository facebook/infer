/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

namespace binary_conditional {

struct X {
  operator bool() { return true; }
};

X getX() {
  X x;
  return x;
}

// more conditional operator tests in C tests
void binaryConditional() {
  X a;
  X x = getX() ?: a;
}

void conditional() {
  X a;
  X x = getX() ? getX() : a;
}
}
