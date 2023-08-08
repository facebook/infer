/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

namespace shared_ptr_semantics {
struct X {
  int field;
  X(int x = 0) : field(x) {}
  int get() { return field; }
};

int constructor0_ok() {
  std::shared_ptr<int> x(new int(42));
  if (*x != 42) {
    // Should not report a NPE here as *x is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int constructor0_bad() {
  int* p = new int(42);
  std::shared_ptr<int> x(p);
  if (*x == 42) {
    // Should report a NPE here as *x is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int move_constructor0_ok() {
  std::shared_ptr<X> x(std::shared_ptr<X>(new X(42)));
  if (x->get() != 42 || x.use_count() != 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}
int move_constructor1_ok() {
  std::shared_ptr<int> x(new int(42));
  std::shared_ptr<int> y(std::move(x));
  if (*y != 42 || y.use_count() != 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int move_constructor2_ok() {
  std::shared_ptr<int> x(new int(42));
  std::shared_ptr<int> y(std::move(x));
  if (x) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int move_constructor3_ok() {
  std::shared_ptr<X> x(new X(42));
  std::shared_ptr<X> y(std::move(x));
  if (x || y->get() != 42 || y.use_count() != 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int move_constructor0_bad() {
  std::shared_ptr<int> x(std::shared_ptr<int>(new int(42)));
  if (*x == 42 && x.use_count() == 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int move_constructor1_bad() {
  std::shared_ptr<int> x(new int(42));
  std::shared_ptr<int> y(std::move(x));
  if (*y == 42 && y.use_count() == 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int move_constructor2_bad() {
  std::shared_ptr<int> x(new int(42));
  std::shared_ptr<int> y(std::move(x));
  if (!x) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int move_constructor3_bad() {
  std::shared_ptr<X> x(new X(42));
  std::shared_ptr<X> y(std::move(x));
  if (!x && y->get() == 42 && y.use_count() == 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int copy_constructor0_ok() {
  std::shared_ptr<int> x(new int(42));
  std::shared_ptr<int> y(x);
  if (*x != 42) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int copy_constructor1_ok() {
  std::shared_ptr<int> w(new int);
  std::shared_ptr<int> x(new int);
  std::shared_ptr<int> y(w);
  if (w.use_count() != 2) {
    int* q = nullptr;
    return *q;
  }
  y = x;
  if (w.use_count() != 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int copy_constructor2_ok() {
  int* p = new int;
  std::shared_ptr<int> x(p);
  x = x;
  if (x.use_count() != 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int copy_constructor3_ok() {
  int* p = new int;
  std::shared_ptr<int> x(p);
  std::shared_ptr<int>& y = x;
  x = y;
  if (x.use_count() != 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int copy_constructor0_bad() {
  std::shared_ptr<int> x;
  std::shared_ptr<int> y(x);
  if (y.use_count() == 0) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int copy_constructor1_bad() {
  std::shared_ptr<int> x;
  std::shared_ptr<int> y(x);
  if (x.use_count() == 0) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int copy_constructor2_bad() {
  std::shared_ptr<int> x(new int(42));
  std::shared_ptr<int> y(x);
  if (*y == 42 && y.use_count() == 2) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int copy_constructor3_bad() {
  auto p = new int(1);
  auto q = new int(2);
  std::shared_ptr<int> x(p);
  std::shared_ptr<int> y(q);
  y = x;
  return *q;
}

int assignment0_bad() {
  std::shared_ptr<int> x(new int(42));
  std::shared_ptr<int> y;
  y = x;
  if (*x == 42) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int use_count0_ok() {
  std::shared_ptr<int> x(new int(42));
  if (x.use_count() != 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int use_count1_ok() {
  std::shared_ptr<int> x(new int(42));
  std::shared_ptr<int> y(x);
  if (x.use_count() != 2) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int use_count2_ok() {
  std::shared_ptr<int> x(new int(42));
  std::shared_ptr<int> y(x);
  std::shared_ptr<int> z(x);
  if (x.use_count() != 3) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int use_count3_ok() {
  std::shared_ptr<int> x(new int(42));
  std::shared_ptr<int> y(x);
  std::shared_ptr<int> z(y);
  if (x.use_count() != 3) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int use_count4_ok() {
  std::shared_ptr<int> x(new int(42));
  std::shared_ptr<int> y(x);
  std::shared_ptr<int> z;
  z = y;
  if (x.use_count() != 3) {
    // Should not report a NPE here as the ref count is 3
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int use_count0_bad() {
  std::shared_ptr<int> x;
  if (x.use_count() == 0) {
    // Should report a NPE here as the ref count is 0
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int use_count1_bad() {
  std::shared_ptr<int> x(new int(42));
  std::shared_ptr<int> y(x);
  std::shared_ptr<int> z;
  z = y;
  if (x.use_count() == 3 && *z == 42) {
    // Should report a NPE here as the ref count is 3
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int use_count2_bad() {
  std::shared_ptr<int> x(new int(42));
  { std::shared_ptr<int> y(x); }
  if (x.use_count() == 1 && *x == 42) {
    // Should report a NPE here as the ref count is 1
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int use_count3_bad() {
  std::shared_ptr<int> x(new int(42));
  {
    std::shared_ptr<int> y(x);
    { std::shared_ptr<int> z(y); }
    if (x.use_count() == 2 && *y == 42) {
      // Should report a NPE here as the ref count is 2
      int* q = nullptr;
      return *q;
    }
  }
  return 0;
}
int use_count4_bad() {
  std::shared_ptr<int> x(nullptr);
  if (x.use_count() == 0) {
    // Should report a NPE here as the ref count is 0
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int get0_ok() {
  int* p = new int(42);
  std::shared_ptr<int> x(p);
  if (x.get() != p) {
    // Should not report a NPE here as x contains the pointer p
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int get1_ok() {
  std::shared_ptr<int> x;
  int* p = x.get(); // no dereference
  if (p) {
    // Should not report a NPE here as p points to nullptr
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int get0_bad() {
  int* p = new int(42);
  std::shared_ptr<int> x(p);
  if (x.get() == p) {
    // Should not report a NPE here as x contains the pointer p
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int get1_bad() {
  std::shared_ptr<int> x;
  int* p = x.get(); // no dereference
  if (!p) {
    // Should report a NPE here as p points to nullptr
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int reset_ptr_ok() {
  std::shared_ptr<int> x(new int(10));
  x.reset();
  return 42;
}

int reset_ptr_use_count0_bad() {
  std::shared_ptr<int> x(new int(10));
  x.reset();
  if (x.use_count() == 0) {
    int* q = nullptr;
    return *q;
  }
  return 42;
}

int reset_ptr_use_count1_bad() {
  auto p = new int(5);
  std::shared_ptr<int> x(p);
  std::shared_ptr<int> y(x);
  y.reset();
  if (x.use_count() == 1) {
    int* q = nullptr;
    return *q;
  }
  return 42;
}

int reset_ptr_use_count2_bad() {
  auto p = new int(5);
  std::shared_ptr<int> x(p);
  std::shared_ptr<int> y(x);
  y.reset();
  if (y.use_count() == 0) {
    int* q = nullptr;
    return *q;
  }
  return 42;
}

int reset_ptr_deref1_ok() {
  std::shared_ptr<int> x;
  x.reset();
  x.reset(new int);
  return *x;
}

int shared_ptr_move_deref_ok() {
  std::shared_ptr<int> p1(new int);
  std::shared_ptr<int> p2 = std::move(p1);
  return *p2;
}

int shared_ptr_move0_bad() {
  std::shared_ptr<int> p1(new int);
  std::shared_ptr<int> p2 = std::move(p1);
  return *p1;
}

int shared_ptr_move_count0_bad() {
  std::shared_ptr<int> p1(new int);
  std::shared_ptr<int> p2;
  p2 = std::move(p1);
  if (p2.use_count() == 1) {
    int* q = nullptr;
    return *q;
  }
  return *p2;
}

int shared_ptr_move_count1_bad() {
  std::shared_ptr<int> p1(new int);
  std::shared_ptr<int> p2;
  p2 = std::move(p1);
  if (p1.use_count() == 0) {
    int* q = nullptr;
    return *q;
  }
  return *p2;
}

int shared_ptr_move_deref_bad() {
  std::shared_ptr<int> p1(new int);
  std::shared_ptr<int> p2;
  p1 = std::move(p2);
  return *p1;
}

struct Foo : public std::enable_shared_from_this<Foo> {
  Foo() {}
  ~Foo() {}
  std::shared_ptr<Foo> getFoo() { return shared_from_this(); }
};

int FP_shared_from_this_ok() {
  Foo* f = new Foo;
  int* q = nullptr;
  std::shared_ptr<Foo> pf1;
  {
    std::shared_ptr<Foo> pf2(f);
    pf1 = pf2->getFoo(); // shares ownership of object with pf2
    if (pf2.use_count() !=
        2) { // should not report a NPE here as the ref count is 2
      return *q;
    }
  }
  if (pf1.use_count() != 1) {
    // pf2 is out of scope
    // should not report a NPE here as the ref count is 1
    return *q;
  }
  return 0;
}

int FN_shared_from_this_bad() {
  Foo* f = new Foo;
  int* q = nullptr;
  std::shared_ptr<Foo> pf1;
  {
    std::shared_ptr<Foo> pf2(f);
    pf1 = pf2->getFoo();
    if (pf2.use_count() == 2 && pf1.use_count() == 2) {
      // should report a NPE here as pf1, pf2 share ownership of f
      return *q;
    }
  }
  return 0;
}

template <typename T>
class PrimaryPtr {
 public:
  std::shared_ptr<T> lock() const {
    if (auto res = weak.lock()) {
      return *res;
    }
    return nullptr;
  }

 private:
  std::weak_ptr<std::shared_ptr<T>> weak;
};

void lock_primary_ptr_ok(PrimaryPtr<std::string> primary) {
  auto x = primary.lock();
}

int call_static_pointer_cast_ok() {
  std::shared_ptr<X> x(new X(42));
  int* p = &std::static_pointer_cast<X>(x)->field;
  return *p;
}
} // namespace shared_ptr_semantics
