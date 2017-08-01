/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
namespace destructor_scope {

struct X {
  ~X() {}
};

struct Y {
  ~Y() {}
};

struct Z {};

struct S {
  X x;
};

void test1(bool a, bool b) {
  X x1;
  S s;
  {
    X x2;
    Y y2;
    if (a) {
      return;
    }
    {
      X x3;
      if (b) {
        return;
      }
    }
  }
  Y y1;
  { Y y3; }
}

int test2(bool a) {
  X x1;
  if (a) {
    X x2;
    return 1;
  } else {
    X x3;
    return 2;
  }
}

X getX() {
  X x;
  return x;
}

Z getZ() {
  Z z;
  return z;
}

/* Having `callgetZ` with a function call to `getZ`
   makes clang to add a destructor ~Z with empty body for `Z`.
   We want to test if we do not inject empty-body destructor
   call in `getZ`. */
void callgetZ() { getZ(); }
} // namespace destructor_scope
