/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
namespace implicit_cast_with_const {

struct Base {
  int* f{nullptr};
};

struct Derived : public Base {};

int deref(const Base& b) { return *b.f; }

int BaseDerefNPE() {
  Base b;
  return deref(b);
}

int DerivedDerefNPE() {
  Derived d;
  return deref(d);
}

int DerivedDerefNoNpe() {
  Derived d;
  int x;
  d.f = &x;
  return deref(d);
}
}
