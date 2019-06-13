/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
} // namespace binary_conditional
