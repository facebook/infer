/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <utility>

namespace move {

struct X {
  X() : f(1) {}
  X(X&& x) {
    f = x.f;
    x.f = 0; // assign zero to field of moved_from object
  }
  int f;
};

int div0_moved_from() {
  X x1;
  X x2 = std::move(x1);
  return 1 / x1.f;
}

int div1_moved_from() {
  X x1;
  X x2 = std::move(x1);
  return 1 / (x1.f + 1);
}

int div1_moved_to() {
  X x1;
  X x2 = std::move(x1);
  return 1 / x2.f;
}

int div0_moved_to() {
  X x1;
  X x2 = std::move(x1);
  return 1 / (x2.f - 1);
}
}
