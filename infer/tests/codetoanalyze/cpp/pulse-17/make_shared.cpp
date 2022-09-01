/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <memory>
#include <string>

namespace make_shared_ptr {

struct X {
  int field;
  int* pointer_field;
  int get() { return field; }
  void set(int value) { field = value; }
  X() {
    pointer_field = new int;
    field = *pointer_field;
  }
  X(int arg) {
    pointer_field = new int(arg);
    field = *pointer_field;
  }
  X(int* arg) {
    pointer_field = arg;
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

int make_shared0_ok() {
  auto x = std::make_shared<int>(42);
  if (*x != 42) {
    // Should not report a NPE here as *x is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int make_shared1_ok() {
  auto x = std::make_shared<int>(42);
  if (x.use_count() != 1) {
    // Should not report a NPE here as ref count is 1
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int make_shared2_ok() {
  int n = 42;
  auto x = std::make_shared<int>(n);
  n = 1;
  if (*x != 42) {
    // Should not report a NPE here as *x is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int make_shared3_ok() {
  auto x = std::make_shared<Integer>(52, 10);
  if (x->get() != 42) {
    // Should not report a NPE here as x->get() is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int make_shared4_ok() {
  auto x = std::make_shared<Y>(Y());
  x->get();
  return 0;
}

int make_shared5_ok() {
  auto obj = Y();
  auto x = std::make_shared<Y>(obj);
  x->get();
  return 0;
}

int make_shared6_ok() {
  const Y obj = Y();
  auto x = std::make_shared<Y>(obj);
  x->get();
  return 0;
}

int make_shared7_ok() {
  auto x = std::make_shared<Integer>(Integer());
  x->get();
  return 0;
}

int make_shared8_ok() {
  auto x = std::make_shared<X>(new int(42));
  if (x->get() != 42) {
    // Should not report a NPE here as x->get() is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int make_shared0_bad() {
  auto x = std::make_shared<int>(42);
  if (*x == 42) {
    // Should report a NPE here as *x is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int make_shared1_bad() {
  int n = 42;
  auto x = std::make_shared<int>(n);
  n = 1;
  if (*x == 42) {
    // Should report a NPE here as *x is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int make_shared2_bad() {
  auto x = std::make_shared<int>(42);
  if (x.use_count() == 1) {
    // Should report a NPE here as ref count is 1
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int make_shared3_bad() {
  auto x = std::make_shared<X>(X());
  x->get(); // Should report a USE_AFTER_LIFETIME here as x contains the same
            // pointer_field as the one of X() which has been deallocated since
            // X() is a temporary expression
  return 0;
}

int make_shared4_bad() {
  auto obj = X();
  auto x = std::make_shared<X>(obj);
  x->get();
  // Should report a USE_AFTER_DELETE here as the destructors of x and X
  // deallocate the same pointer twice
  return 0;
}

int make_shared5_bad() {
  auto x = std::make_shared<Integer>(52, 10);
  if (x->get() == 42) {
    // Should report a NPE here as x->get() is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int make_shared6_bad() {
  int v = 42;
  auto x = std::make_shared<int>(v);
  v++;
  if (*x == 42) {
    // Should report a NPE here as *x is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int make_shared_ptr_use_ok() {
  std::shared_ptr<X> x = std::make_shared<X>();
  return x->get();
}

struct B {
  ~B();
};

struct A {
  ~A() { (void)*f; }
  const B* f;
};

void allocate_in_branch_ok(bool b) {
  std::shared_ptr<A> a1;
  std::shared_ptr<A> a2;
  std::shared_ptr<A>* a3 = &a1;

  if (b) {
    a2 = std::make_shared<A>();
    a3 = &a2;
  }

  if (b) {
    const B* read = (*a3)->f;
  }
}
} // namespace make_shared_ptr
