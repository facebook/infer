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
  int* pointer_field;
  int get() { return *pointer_field; }
  void set(int value) { *pointer_field = value; }
  X() {
    pointer_field = new int;
  }
  X(int arg) {
    pointer_field = new int(arg);
  }
  X(int* arg) {
    pointer_field = arg;
  }
  ~X() { delete pointer_field; }
};

struct Y {
  int* pointer_field;
  int get() { return *pointer_field; }
  void set(int value) { *pointer_field = value; }
  Y() {
    pointer_field = new int;
  }
  Y(const Y& other) { pointer_field = new int(*(other.pointer_field)); }
  ~Y() { delete pointer_field; }
};

struct SharedPtr {
  SharedPtr(std::shared_ptr<int> _s_ptr) { s_ptr0 = _s_ptr; }
  SharedPtr(std::shared_ptr<float> _s_ptr) : s_ptr1(_s_ptr) {}
  std::shared_ptr<int> s_ptr0;
  std::shared_ptr<float> s_ptr1;
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

int make_shared9_ok() {
  auto sharedPtr = SharedPtr(std::make_shared<int>(42));
  return *sharedPtr.s_ptr0;
}

float make_shared10_ok() {
  auto sharedPtr = SharedPtr(std::make_shared<float>(42));
  return *sharedPtr.s_ptr1;
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

int take_shared_ptr_as_arg(std::shared_ptr<int> s_ptr) { return 0; }

std::unique_ptr<int> take_shared_ptr_get_fresh_unique_ptr(
    std::shared_ptr<int> s_ptr) {
  take_shared_ptr_as_arg(s_ptr);
  return std::unique_ptr<int>(new int(42));
}

int passed_shared_ptr_around_ok(std::shared_ptr<int> s_ptr) {
  std::unique_ptr<int> u_ptr = take_shared_ptr_get_fresh_unique_ptr(s_ptr);
  return *u_ptr;
}

std::unique_ptr<int> get_unique_ptr_from_shared(std::shared_ptr<int> s_ptr) {
  // takes a shared_ptr and creates a unique_ptr that manage the same pointer
  return std::unique_ptr<int>(s_ptr.get());
}

int doubly_managed_ptr_bad() {
  std::shared_ptr<int> s_ptr = std::make_shared<int>(42);
  std::unique_ptr<int> u_ptr = get_unique_ptr_from_shared(s_ptr);
  // u_ptr and s_ptr manage the same pointer, which is deleted twice
  return *u_ptr;
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
