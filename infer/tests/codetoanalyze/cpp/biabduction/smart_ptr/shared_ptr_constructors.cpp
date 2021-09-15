/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>
#include <stdexcept>

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

void FN_get_from_base1_nullptr_deref_bad() {
  Base b = *(getFromBase1(nullptr));
}

void FN_get_from_base2_nullptr_deref_bad() {
  Base b = *(getFromBase2(nullptr));
}

void FN_get_from_derived1_nullptr_deref_bad() {
  Base b = *(getFromDerived1(nullptr));
}

void FN_get_from_derived2_nullptr_deref_bad() {
  Base b = *(getFromDerived2(nullptr));
}

void FN_get_from_derived3_nullptr_deref_bad() {
  Base b = *(getFromDerived3(nullptr));
}

void FN_get_from_base1_null_f1_deref_bad() {
  Base b;
  int v;
  b.f1 = &v;
  std::shared_ptr<Base> p = getFromBase1(&b);
  b.f1 = nullptr;
  int r = *(p->f1);
}

void FN_get_from_base2_null_f1_deref_bad() {
  Base b;
  int v;
  b.f1 = &v;
  std::shared_ptr<Base> p = getFromBase2(&b);
  b.f1 = nullptr;
  int r = *(p->f1);
}

void FN_get_from_derived1_null_f1_deref_bad() {
  Derived b;
  int v;
  b.f1 = &v;
  std::shared_ptr<Base> p = getFromDerived1(&b);
  b.f1 = nullptr;
  int r = *(p->f1);
}

void FN_get_from_derived2_null_f1_deref_bad() {
  Derived b;
  int v;
  b.f1 = &v;
  std::shared_ptr<Base> p = getFromDerived2(&b);
  b.f1 = nullptr;
  int r = *(p->f1);
}

void FN_get_from_derived3_null_f1_deref_bad() {
  Derived b;
  int v;
  b.f1 = &v;
  std::shared_ptr<Base> p = getFromDerived3(&b);
  b.f1 = nullptr;
  int r = *(p->f1);
}

struct A {
  void baz();
};
struct B {
  A* a;
};

std::shared_ptr<B> external_def();
std::shared_ptr<B> internal_null_def() {
  // TODO: do the same test for std::make_shared
  // We can't use std::make_shared here because it will cause a memory leak to
  // be reported instead. In the future we probably need to use something like
  // __set_wont_leak_attribute() to suppress the leak report.
  auto r = external_def();
  r->a = nullptr;
  return r;
}

std::shared_ptr<A> ERROR_aliasing_construct_from_external() {
  auto p = external_def();
  if (!p)
    throw std::logic_error("Suppress NULL");
  return {p, p->a};
}
std::shared_ptr<A> ERROR_aliasing_construct_from_internal() {
  auto p = internal_null_def();
  if (!p)
    throw std::logic_error("Suppress NULL");
  return {p, p->a};
}

void aliasing_member_not_null_ok() {
  auto q = ERROR_aliasing_construct_from_external();
  // q is unknown here so we should not report null deref
  // Also we should not report dangling pointer because q is still alive
  q->baz();
}
void aliasing_member_null_bad() {
  auto q = ERROR_aliasing_construct_from_internal();
  // q is known here so we should report null deref
  // Also we should not report dangling pointer because q is still alive
  q->baz();
}
} // namespace shared_ptr_constructors
