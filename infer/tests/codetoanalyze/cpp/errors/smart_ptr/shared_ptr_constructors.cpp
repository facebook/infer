/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <memory>

namespace shared_ptr_constructors {

struct Base {
  int* f1;
};

struct Derived : public Base {
  int* f2;
};

std::shared_ptr<Base> getFromBase1(Base* b) { return std::shared_ptr<Base>(b); }

std::shared_ptr<Base> getFromBase2(Base* b) {
  std::shared_ptr<Base> result;
  result = std::shared_ptr<Base>(b); // assignment operator
  return result;
}

std::shared_ptr<Base> getFromDerived1(Derived* d) {
  return std::shared_ptr<Base>(d);
}

std::shared_ptr<Base> getFromDerived2(Derived* d) {
  std::shared_ptr<Derived> sd(d);
  return std::shared_ptr<Base>(sd);
}

std::shared_ptr<Base> getFromDerived3(Derived* d) {
  std::shared_ptr<Derived> sd(d);
  std::shared_ptr<Base> result;
  result = sd; // assignment operator
  return result;
}

void get_from_base1_nullptr_deref() { Base b = *(getFromBase1(nullptr)); }

void get_from_base2_nullptr_deref() { Base b = *(getFromBase2(nullptr)); }

void get_from_derived1_nullptr_deref() { Base b = *(getFromDerived1(nullptr)); }

void get_from_derived2_nullptr_deref() { Base b = *(getFromDerived2(nullptr)); }

void get_from_derived3_nullptr_deref() { Base b = *(getFromDerived3(nullptr)); }

void get_from_base1_null_f1_deref() {
  Base b;
  int v;
  b.f1 = &v;
  std::shared_ptr<Base> p = getFromBase1(&b);
  b.f1 = nullptr;
  int r = *(p->f1);
}

void get_from_base2_null_f1_deref() {
  Base b;
  int v;
  b.f1 = &v;
  std::shared_ptr<Base> p = getFromBase2(&b);
  b.f1 = nullptr;
  int r = *(p->f1);
}

void get_from_derived1_null_f1_deref() {
  Derived b;
  int v;
  b.f1 = &v;
  std::shared_ptr<Base> p = getFromDerived1(&b);
  b.f1 = nullptr;
  int r = *(p->f1);
}

void get_from_derived2_null_f1_deref() {
  Derived b;
  int v;
  b.f1 = &v;
  std::shared_ptr<Base> p = getFromDerived2(&b);
  b.f1 = nullptr;
  int r = *(p->f1);
}

void get_from_derived3_null_f1_deref() {
  Derived b;
  int v;
  b.f1 = &v;
  std::shared_ptr<Base> p = getFromDerived3(&b);
  b.f1 = nullptr;
  int r = *(p->f1);
}
}
