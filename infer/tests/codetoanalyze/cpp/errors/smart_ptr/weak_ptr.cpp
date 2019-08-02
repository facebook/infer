/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

extern "C" {
void __infer_fail(char*);
}

namespace weak_ptr_constructors {

struct Base {
  int* f1;
  Base(int* f1 = nullptr) : f1(f1) {}
};

struct Derived : public Base {
  int* f2;
  Derived(int* f1 = nullptr) : Base(f1) {}
};

std::weak_ptr<Base> empty() { return std::weak_ptr<Base>(); }

std::weak_ptr<Base> fromWeakBaseConstr(std::weak_ptr<Base> b) {
  return std::weak_ptr<Base>(b);
}

std::weak_ptr<Base> fromWeakBaseAssign(std::weak_ptr<Base> b) {
  std::weak_ptr<Base> result;
  result = b;
  return result;
}

std::weak_ptr<Base> fromWeakDerivedConstr(std::weak_ptr<Derived> d) {
  return std::weak_ptr<Base>(d);
}

std::weak_ptr<Base> fromWeakDerivedAssign(std::weak_ptr<Derived> d) {
  std::weak_ptr<Base> result;
  result = d;
  return result;
}

std::weak_ptr<Base> fromSharedBaseConstr(std::shared_ptr<Base> b) {
  return std::weak_ptr<Base>(b);
}

std::weak_ptr<Base> fromSharedBaseAssign(std::shared_ptr<Base> b) {
  std::weak_ptr<Base> result;
  result = b;
  return result;
}

std::weak_ptr<Base> fromSharedDerivedConstr(std::shared_ptr<Derived> d) {
  return std::weak_ptr<Base>(d);
}

std::weak_ptr<Base> fromSharedDerivedConstr2(std::shared_ptr<Derived> d) {
  std::weak_ptr<Derived> sd(d);
  return std::weak_ptr<Base>(sd);
}

std::weak_ptr<Base> fromSharedDerivedAssign(std::shared_ptr<Derived> d) {
  std::weak_ptr<Derived> sd(d);
  std::weak_ptr<Base> result;
  result = sd;
  return result;
}
} // namespace weak_ptr_constructors

namespace weak_ptr_derefs {
using namespace weak_ptr_constructors;

int safeGetFromEmpty_good() {
  auto w = empty();
  auto s = w.lock();
  while (!s)
    ;
  return *s->f1; // never reached
}

std::shared_ptr<Base> safeGet(std::weak_ptr<Base> p) {
  auto s = p.lock();
  while (!s)
    ;
  return s;
}

int FN_safeGetFromWeakBaseConstr_bad(int v) {
  auto b = std::make_shared<Base>(&v);
  auto s = safeGet(fromWeakBaseConstr(std::weak_ptr<Base>(b)));
  b->f1 = nullptr;
  return *s->f1;
}

int FN_safeGetFromWeakBaseAssign_bad(int v) {
  auto b = std::make_shared<Base>(&v);
  auto s = safeGet(fromWeakBaseAssign(std::weak_ptr<Base>(b)));
  b->f1 = nullptr;
  return *s->f1;
}

int FN_safeGetFromWeakDerivedConstr_bad(int v) {
  auto d = std::make_shared<Derived>(&v);
  auto s = safeGet(fromWeakDerivedConstr(std::weak_ptr<Derived>(d)));
  d->f1 = nullptr;
  return *s->f1;
}

int FN_safeGetFromWeakDerivedAssign_bad(int v) {
  auto d = std::make_shared<Derived>(&v);
  auto s = safeGet(fromWeakDerivedAssign(std::weak_ptr<Derived>(d)));
  d->f1 = nullptr;
  return *s->f1;
}

int FN_safeGetFromSharedBaseConstr_bad(int v) {
  auto b = std::make_shared<Base>(&v);
  auto s = safeGet(fromSharedBaseConstr(b));
  b->f1 = nullptr;
  return *s->f1;
}

int FN_safeGetFromSharedBaseAssign_bad(int v) {
  auto b = std::make_shared<Base>(&v);
  auto s = safeGet(fromSharedBaseAssign(b));
  b->f1 = nullptr;
  return *s->f1;
}

int FN_safeGetFromSharedDerivedConstr_bad(int v) {
  auto b = std::make_shared<Derived>(&v);
  auto s = safeGet(fromSharedDerivedConstr(b));
  b->f1 = nullptr;
  return *s->f1;
}

int FN_safeGetFromSharedDerivedConstr2_bad(int v) {
  auto b = std::make_shared<Derived>(&v);
  auto s = safeGet(fromSharedDerivedConstr2(b));
  b->f1 = nullptr;
  return *s->f1;
}

int FN_safeGetFromSharedDerivedAssign_bad(int v) {
  auto b = std::make_shared<Derived>(&v);
  auto s = safeGet(fromSharedDerivedAssign(b));
  b->f1 = nullptr;
  return *s->f1;
}
} // namespace weak_ptr_derefs

namespace weak_ptr_modifiers {

void reset(std::weak_ptr<int>& p) { p.reset(); }

void swap(std::weak_ptr<int>& p) {
  std::weak_ptr<int> q;
  q.swap(p);
}
} // namespace weak_ptr_modifiers

namespace weak_ptr_observers {
using namespace weak_ptr_constructors;

long use_count(std::weak_ptr<int>& p) { return p.use_count(); }

void use_count_empty_bad() {
  std::weak_ptr<int> p;
  if (p.use_count() == 0) {
    __infer_fail("use_count on empty weak_ptr is 0");
  }
}

void use_count_after_reset_bad(std::weak_ptr<int>& p) {
  p.reset();
  if (p.use_count() == 0) {
    __infer_fail("use_count after weak_ptr reset is 0");
  }
}

bool expired(std::weak_ptr<int>& p) { return p.expired(); }

void expired_empty_bad() {
  std::weak_ptr<int> p;
  if (p.expired()) {
    __infer_fail("expired on empty weak_ptr is true");
  }
}

void expired_after_reset_bad(std::weak_ptr<int>& p) {
  p.reset();
  if (p.expired()) {
    __infer_fail("expired after weak_ptr reset is true");
  }
}

void expired_after_swap_bad(std::weak_ptr<int>& p) {
  std::weak_ptr<int> q;
  q.swap(p);
  if (p.expired()) {
    __infer_fail("expired after weak_ptr swap with empty is true");
  }
}

std::shared_ptr<int> lock(std::weak_ptr<int>& p) { return p.lock(); }

void empty_weak_lock_returns_null_bad() {
  std::weak_ptr<int> p;
  auto s = p.lock();
  int _ = *s.get();
}

void expired_means_null_bad(std::weak_ptr<int>& p) {
  if (p.expired()) {
    auto s = p.lock();
    int _ = *s.get();
  }
}

void lock_can_be_null_bad(std::weak_ptr<int>& p) {
  auto s = p.lock();
  int _ = *s.get();
}

int safe_deref_ok(std::weak_ptr<int>& p) {
  if (auto s = p.lock()) {
    return *s.get();
  }
  return 0;
}

std::shared_ptr<int> shared_still_in_scope_good_FP() {
  /* It's not a big issue to FP in this case.
    Code should not be written like that anyway. */
  auto s = std::make_shared<int>();
  auto p = std::weak_ptr<int>(s);
  auto s2 = p.lock();
  auto _ = *s2.get();
  return s;
}

bool owner_before(std::weak_ptr<Base>& p, std::weak_ptr<Base>& q) {
  return p.owner_before(q);
}

bool owner_before(std::weak_ptr<Base>& p, std::shared_ptr<Derived>& q) {
  return p.owner_before(q);
}
} // namespace weak_ptr_observers
