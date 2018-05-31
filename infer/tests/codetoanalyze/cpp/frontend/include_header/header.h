/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct A {
  int div0() { return 1 / 0; }
};

template <class T>
struct B {
  int div0() { return 1 / 0; }
};

int div0_fun() { return 1 / 0; }

template <class T>
int div0_templ() {
  return 1 / 0;
}
