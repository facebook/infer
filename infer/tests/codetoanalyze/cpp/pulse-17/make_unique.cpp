/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <memory>

namespace make_unique_ptr {

struct X {
  int field;
  int* pointer_field;
  int get() { return field; }
  void set(int value) { field = value; }
  X() {
    pointer_field = new int;
    field = *pointer_field;
  }
  ~X() { delete pointer_field; }
};

struct Y {
  int field;
  int* pointer_field;
  int get() { return field; }
  void set(int value) { field = value; }
  Y() {
    pointer_field = new int;
    field = *pointer_field;
  }
  Y(const Y& other) { pointer_field = new int(*(other.pointer_field)); }
  ~Y() { delete pointer_field; }
};

struct Integer {
  int field;
  int get() { return field; }
  void set(int value) { field = value; }
  Integer(int value = 0) { field = value; }
  Integer(Integer&& other) {
    field = other.field;
    other.field = 0;
  }
  Integer(int value1, int value2) { field = value1 - value2; }
  Integer(X* x) { field = x->get(); }
};

int make_unique0_ok() {
  auto x = std::make_unique<int>(42);
  if (*x != 42) {
    // Should not report a NPE here as *x is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int make_unique1_ok() {
  int obj = 5;
  auto x = std::make_unique<int>(obj);
  return *x;
}

int make_unique2_ok() {
  auto x = std::make_unique<Integer>(52, 10);
  if (x->get() != 42) {
    // Should not report a NPE here as x->get() is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int make_unique3_ok() {
  auto x = std::make_unique<Integer>(Integer());
  x->get();
  return 0;
}

int make_unique4_ok() {
  auto x = std::make_unique<Y>(Y());
  x->get();
  return 0;
}

int make_unique5_ok() {
  auto obj = Y();
  auto x = std::make_unique<Y>(obj);
  x->get();
  return 0;
}

int make_unique6_ok() {
  const Y obj = Y();
  auto x = std::make_unique<Y>(obj);
  x->get();
  return 0;
}

int make_unique0_bad() {
  auto x = std::make_unique<int>(42);
  if (*x == 42) {
    // Should report a NPE here as *x is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int make_unique1_bad() {
  auto x = std::make_unique<X>(X());
  x->get(); // Should report a USE_AFTER_LIFETIME here as x contains the same
            // pointer_field as the one of X() which has been deallocated since
            // X() is a temporary expression
  return 0;
}

int make_unique2_bad() {
  auto obj = X();
  auto x = std::make_unique<X>(obj);
  x->get();
  // Should report a USE_AFTER_DELETE here as the destructors of x and X
  // deallocate the same pointer twice
  return 0;
}

int make_unique3_bad() {
  int v = 42;
  auto x = std::make_unique<int>(v);
  v++;
  if (*x == 42) {
    // Should report a NPE here as *x is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int make_unique_ptr_use_ok() {
  std::unique_ptr<X> x = std::make_unique<X>();
  return x->get();
}

void make_unique_ptr_release_bad() {
  std::unique_ptr<X> x = std::make_unique<X>();
  x.release();
}

struct B {
  ~B();
};

struct A {
  ~A() { (void)*f; }
  const B* f;
};

void allocate_in_branch_ok(bool b) {
  std::unique_ptr<A> a1;
  std::unique_ptr<A> a2;
  std::unique_ptr<A>* a3 = &a1;

  if (b) {
    a2 = std::make_unique<A>();
    a3 = &a2;
  }

  if (b) {
    const B* read = (*a3)->f;
  }
}

A copy_elision_unique_ptr_ok() {
  static std::shared_ptr<A> ptr(std::make_unique<A>());
  return *ptr;
}

} // namespace make_unique_ptr
