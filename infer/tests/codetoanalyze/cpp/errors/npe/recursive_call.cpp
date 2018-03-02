/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

namespace recursive_call {

struct A {
  A() { f = 0; }
  int f;
  A* getptr();
  A* mk();
};

struct F {
  A* mk();
};

int rec(A* a = nullptr) {
  if (a == nullptr) {
    A* a = new A();
    int r = rec(a);
    delete a;
    return r;
  }

  return a->f;
};

int test_rec_ok() {
  return rec(); // infer should not report here
}

A* rec2(F f, A* a = nullptr) {
  if (a == nullptr) {
    auto tmp = f.mk();
    return rec2(
        f, tmp); // assertion failure because of abducedRefParam0 in `getptr`
  }
  return a->getptr();
}

int test_rec2_ok() {
  F f;
  return rec2(f)->f;
}

} // namespace recursive_call
