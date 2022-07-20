/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

namespace unique_ptr {

int constructor0_ok() {
  std::unique_ptr<int> x(new int(42));
  if (*x != 42) {
    // Should not report a NPE here as *x is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int constructor0_bad() {
  int* p = new int(42);
  std::unique_ptr<int> x(p);
  if (*x == 42) {
    // Should report a NPE here as *x is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int destructor0_ok() {
  auto x = new std::unique_ptr<int>(new int(5));
  delete x;
  return 0;
}

int destructor1_ok() {
  { std::unique_ptr<int>(new int(5)); }
  return 0;
}

int destructor2_ok() {
  { std::unique_ptr<int> p; }
  return 0;
}

int FP_destructor3_ok() {
  auto x = new std::unique_ptr<int>();
  delete x;
  return 0;
}

int destructor0_bad() {
  auto p = new int(5);
  auto x = new std::unique_ptr<int>(p);
  delete x;
  // Should report a NPE here as p has been deallocated
  return *p;
}

int destructor1_bad() {
  auto p = new int(5);
  { std::unique_ptr<int> x(p); }
  // Should report a NPE here as p has been deallocated
  return *p;
}

int array_access0_ok() {
  auto p = new int[3];
  p[1] = 42;
  std::unique_ptr<int[]> x(p);
  if (x[1] != 42) { // Should not report a NPE here as x[1] is equal to 42
    int* q = nullptr;
    *q;
  }
  return 0;
}

int array_access1_ok() {
  auto p = new int[3];
  p[1] = 42;
  std::unique_ptr<int[]> x(p);
  if (x[1] != 42) { // Should not report a NPE here as x[1] is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int array_access0_bad() {
  std::unique_ptr<int[]> x(new int[3]);
  x[1] = 42;
  if (x[1] == 42) { // Should report a NPE here as x[1] is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int array_access1_bad() {
  auto p = new int[3];
  p[1] = 42;
  std::unique_ptr<int[]> x(p);
  if (x[1] == 42) { // Should report a NPE here as x[1] is equal to 42
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int get0_ok() {
  int* p = new int(42);
  std::unique_ptr<int> x(p);
  if (x.get() != p) {
    // Should not report a NPE here as x contains the pointer p
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int get1_ok() {
  std::unique_ptr<int> x;
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
  std::unique_ptr<int> x(p);
  if (x.get() == p) {
    // Should not report a NPE here as x contains the pointer p
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int get1_bad() {
  std::unique_ptr<int> x;
  int* p = x.get(); // no dereference
  if (!p) {
    // Should report a NPE here as p points to nullptr
    int* q = nullptr;
    return *q;
  }
  return 0;
}

struct X {
  int field;
  int* pointer_field;
  int get() { return field; }
  void set(int value) { field = value; }
  X() { pointer_field = new int; }
  ~X() { delete pointer_field; }
};

int empty_ptr_deref_bad() {
  std::unique_ptr<int> x;
  return *x;
}

int empty_array_ptr_deref_bad() {
  std::unique_ptr<int[]> x;
  return x[0];
}

int nullptr_ptr_deref_bad() {
  std::unique_ptr<int> x(nullptr);
  return *x;
}

int nullptr_array_ptr_deref_bad() {
  std::unique_ptr<int[]> x(nullptr);
  return x[2];
}

int empty_ptr_field_deref_bad() {
  std::unique_ptr<X> x;
  return x.get()->field;
}

int empty_ptr_field_deref2_bad() {
  std::unique_ptr<X> x;
  return x->field;
}

int empty_ptr_method_deref_bad() {
  std::unique_ptr<X> x;
  return x->get();
}

int unique_ptr_create_use_ok() {
  std::unique_ptr<X> x(new X());
  return x->get();
}

void unique_ptr_release0_bad() {
  std::unique_ptr<X> x(new X());
  x.release();
  *x; // Should report a NPE here as x, after the release, contains a nullptr
}

void FN_unique_ptr_release1_bad() {
  std::unique_ptr<X> x(new X());
  x.release(); // Should report a memory leak since the object of X has not been
               // deallocated
}

int reset_ptr_null_deref_bad() {
  std::unique_ptr<int> x(new int);
  x.reset();
  return *x;
}

void unique_ptr_release_get_ok() {
  std::unique_ptr<X> x(new X());
  if (x.get() != x.release()) {
    // Should not report a NPE here as both x.release() and x.get() return the
    // pointers contained in x
    *x; // x, after the release, contains a nullptr
  }
}

void unique_ptr_release_get_bad() {
  std::unique_ptr<X> x(new X());
  if (x.get() == x.release())
  // Should report a NPE here as both x.release() and x.get() return the
  // pointers contained in x
  {
    *x; // x, after the release, contains a nullptr
  }
}

int reset_ptr_null_deref2_bad() {
  std::unique_ptr<int> x(new int);
  x.reset(new int);
  x.reset();
  return *x;
}

int reset_ptr_null_deref_ok() {
  std::unique_ptr<int> x(new int);
  x.reset();
  return 42;
}

int reset_ptr_deref_ok() {
  std::unique_ptr<int> x;
  x.reset(new int);
  return *x;
}

int reset_ptr_deref2_ok() {
  std::unique_ptr<int> x;
  x.reset();
  x.reset(new int);
  return *x;
}

int unique_ptr_copy_null_deref_bad() {
  std::unique_ptr<int> p1;
  std::unique_ptr<int> p2 = std::move(p1);
  return *p2;
}

int unique_ptr_assign_null_deref_bad() {
  std::unique_ptr<int> p1(new int);
  std::unique_ptr<int> p2;
  p1 = std::move(p2);
  return *p1;
}

int unique_ptr_move_deref_ok() {
  std::unique_ptr<int> p1(new int);
  std::unique_ptr<int> p2 = std::move(p1);
  return *p2;
}

int unique_ptr_assign_deref_ok() {
  std::unique_ptr<int> p1(new int);
  std::unique_ptr<int> p2;
  p2 = std::move(p1);
  p1.reset();
  return *p2;
}

int unique_ptr_move_null_deref_bad() {
  std::unique_ptr<int> p1(new int);
  std::unique_ptr<int> p2 = std::move(p1);
  return *p1;
}

// Deleter that doesn't actually call delete
template <class T>
struct Deleter {
  using pointer = T*;

  void operator()(pointer ptr) const {}
};

// leaky because the deleter doesn't delete the managed pointer
template <class T>
using leaky_unique_ptr = std::unique_ptr<T, Deleter<T>>;

void no_delete_in_leaky_unique_ptr_bad() { leaky_unique_ptr<X> p(new X()); }

} // namespace unique_ptr
