/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

struct X {
  X() {}
};
const X global;

X test() { return global; }

static const int v = 2;

int test2() {
  int local = v;
  return v;
}
