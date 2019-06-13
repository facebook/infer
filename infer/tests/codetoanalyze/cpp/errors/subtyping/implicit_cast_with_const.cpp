/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
} // namespace implicit_cast_with_const
