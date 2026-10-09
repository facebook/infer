/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

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

int safeGetFromWeakBaseConstr_bad(int v) {
  auto b = std::make_shared<Base>(&v);
  auto s = safeGet(fromWeakBaseConstr(std::weak_ptr<Base>(b)));
  b->f1 = nullptr;
  return *s->f1;
}

int safeGetFromWeakBaseAssign_bad(int v) {
  auto b = std::make_shared<Base>(&v);
  auto s = safeGet(fromWeakBaseAssign(std::weak_ptr<Base>(b)));
  b->f1 = nullptr;
  return *s->f1;
}

int safeGetFromWeakDerivedConstr_bad(int v) {
  auto d = std::make_shared<Derived>(&v);
  auto s = safeGet(fromWeakDerivedConstr(std::weak_ptr<Derived>(d)));
  d->f1 = nullptr;
  return *s->f1;
}

int safeGetFromWeakDerivedAssign_bad(int v) {
  auto d = std::make_shared<Derived>(&v);
  auto s = safeGet(fromWeakDerivedAssign(std::weak_ptr<Derived>(d)));
  d->f1 = nullptr;
  return *s->f1;
}

int safeGetFromSharedBaseConstr_bad(int v) {
  auto b = std::make_shared<Base>(&v);
  auto s = safeGet(fromSharedBaseConstr(b));
  b->f1 = nullptr;
  return *s->f1;
}

int safeGetFromSharedBaseAssign_bad(int v) {
  auto b = std::make_shared<Base>(&v);
  auto s = safeGet(fromSharedBaseAssign(b));
  b->f1 = nullptr;
  return *s->f1;
}

int safeGetFromSharedDerivedConstr_bad(int v) {
  auto b = std::make_shared<Derived>(&v);
  auto s = safeGet(fromSharedDerivedConstr(b));
  b->f1 = nullptr;
  return *s->f1;
}

int safeGetFromSharedDerivedConstr2_bad(int v) {
  auto b = std::make_shared<Derived>(&v);
  auto s = safeGet(fromSharedDerivedConstr2(b));
  b->f1 = nullptr;
  return *s->f1;
}

int safeGetFromSharedDerivedAssign_bad(int v) {
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
    int* x = nullptr;
    *x = 42;
  }
}

void use_count_after_reset_bad(std::weak_ptr<int>& p) {
  p.reset();
  if (p.use_count() == 0) {
    int* x = nullptr;
    *x = 42;
  }
}

bool expired(std::weak_ptr<int>& p) { return p.expired(); }

void expired_empty_bad() {
  std::weak_ptr<int> p;
  if (p.expired()) {
    int* x = nullptr;
    *x = 42;
  }
}

void expired_after_reset_bad(std::weak_ptr<int>& p) {
  p.reset();
  if (p.expired()) {
    int* x = nullptr;
    *x = 42;
  }
}

void expired_after_swap_bad(std::weak_ptr<int>& p) {
  std::weak_ptr<int> q;
  q.swap(p);
  if (p.expired()) {
    int* x = nullptr;
    *x = 42;
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

std::shared_ptr<int> shared_still_in_scope_good() {
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

namespace weak_ptr_lifetime {

struct Callback {
  void run();
};

class Service {
 public:
  void notify_bad() { callback_.lock()->run(); }

  void notify_ok() {
    if (auto callback = callback_.lock()) {
      callback->run();
    }
  }

 private:
  std::weak_ptr<Callback> callback_;
};

void lock_after_owner_destroyed_bad() {
  std::weak_ptr<int> p;
  {
    auto s = std::make_shared<int>(0);
    p = s;
  }
  *p.lock() = 1;
}

void lock_after_loop_bad(int n) {
  std::weak_ptr<int> p;
  for (int i = 0; i < n; i++) {
    auto s = std::make_shared<int>(i);
    p = s;
  }
  *p.lock() = 1;
}

void lock_after_owner_destroyed_checked_ok() {
  std::weak_ptr<int> p;
  {
    auto s = std::make_shared<int>(0);
    p = s;
  }
  if (auto s = p.lock()) {
    *s = 1;
  }
}

void expired_after_owner_destroyed_ok() {
  std::weak_ptr<int> p;
  {
    auto s = std::make_shared<int>(0);
    p = s;
  }
  if (!p.expired()) {
    int* x = nullptr;
    *x = 42;
  }
}

void use_count_while_owner_alive_ok() {
  auto s = std::make_shared<int>(0);
  std::weak_ptr<int> p = s;
  if (p.use_count() != 1) {
    int* x = nullptr;
    *x = 42;
  }
}

void lock_while_owner_alive_ok() {
  auto s = std::make_shared<int>(0);
  std::weak_ptr<int> p = s;
  {
    auto locked = p.lock();
    *locked = 1;
  }
  *s = 2;
}

void owner_destroyed_while_locked_ok() {
  auto s = std::make_shared<int>(0);
  std::weak_ptr<int> p = s;
  auto locked = p.lock();
  s.reset();
  *locked = 1;
}

void lock_from_shared_param_ok(const std::shared_ptr<int>& s) {
  std::weak_ptr<int> p(s);
  if (s) {
    *p.lock() = 1;
  }
}

void shared_from_empty_weak_throws_ok() {
  std::weak_ptr<int> p;
  std::shared_ptr<int> s(p); // throws std::bad_weak_ptr
  int* x = nullptr;
  *x = 42;
}

void shared_from_weak_not_null_ok(const std::weak_ptr<int>& p) {
  std::shared_ptr<int> s(p);
  *s = 1;
}

void lock_and_check(const std::weak_ptr<int>& p) {
  if (auto s = p.lock()) {
    *s = 1;
  }
}

void lock_in_callee_after_owner_destroyed_ok() {
  std::weak_ptr<int> p;
  {
    auto s = std::make_shared<int>(0);
    p = s;
  }
  lock_and_check(p);
}

std::shared_ptr<int> get_locked(const std::weak_ptr<int>& p) {
  return p.lock();
}

void lock_in_callee_latent(const std::weak_ptr<int>& p) { *get_locked(p) = 1; }

void discard(std::shared_ptr<int> s) { (void)s; }

void discard_ref(const std::shared_ptr<int>& s) { (void)s; }

// `(void)s` in `discard` and `discard_ref` is modelled as an unknown call on
// `s`, which forgets the reference count of `a`, so lock() may return null
void FP_lock_after_discarding_copy_ok() {
  auto a = std::make_shared<int>(0);
  std::weak_ptr<int> p = a;
  discard(a);
  *p.lock() = 1;
  *a = 2;
}

void FP_lock_after_discarding_ref_ok() {
  auto a = std::make_shared<int>(0);
  std::weak_ptr<int> p = a;
  discard_ref(a);
  *p.lock() = 1;
}

void FN_lock_after_move_bad() {
  auto s = std::make_shared<int>(0);
  std::weak_ptr<int> p = s;
  std::weak_ptr<int> q = std::move(p);
  // p is empty after the move, but moves are modelled as copies
  *p.lock() = 1;
}

void swap_then_reset_ok() {
  auto a = std::make_shared<int>(0);
  std::shared_ptr<int> b;
  std::weak_ptr<int> p = a;
  a.swap(b);
  a.reset();
  *p.lock() = 1;
}

void std_swap_then_reset_ok() {
  auto a = std::make_shared<int>(0);
  std::shared_ptr<int> b;
  std::weak_ptr<int> p = a;
  std::swap(a, b);
  a.reset();
  *p.lock() = 1;
}

void swap_then_reset_owner_bad() {
  auto a = std::make_shared<int>(0);
  std::shared_ptr<int> b;
  std::weak_ptr<int> p = a;
  a.swap(b);
  b.reset();
  *p.lock() = 1;
}

// move-assigning to the last owner does not release the object it owned, so
// weak pointers to that object do not expire
void FN_move_assign_over_last_owner_bad() {
  auto s = std::make_shared<int>(0);
  std::weak_ptr<int> p = s;
  auto t = std::make_shared<int>(1);
  s = std::move(t);
  *p.lock() = 1;
}

void FN_make_shared_reassign_bad() {
  auto s = std::make_shared<int>(0);
  std::weak_ptr<int> p = s;
  s = std::make_shared<int>(1);
  *p.lock() = 1;
}
} // namespace weak_ptr_lifetime
