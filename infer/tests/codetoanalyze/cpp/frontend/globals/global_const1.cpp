/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
