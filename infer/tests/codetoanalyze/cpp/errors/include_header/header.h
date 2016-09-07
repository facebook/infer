/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

namespace header {

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
}
