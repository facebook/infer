/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

namespace unique_ptr {

struct X {
  int field;
  int* pointer_field;
  int get() { return field; }
  void set(int value) { field = value; }
  X() { pointer_field = new int; }
  ~X() { delete pointer_field; }
};

int empty_ptr_access() {
  std::unique_ptr<int> x;
  int* p = x.get(); // no dereference
  if (p) {
    return 1;
  }
  return 0;
}

int FN_empty_ptr_deref_bad() {
  std::unique_ptr<int> x;
  return *x;
}

int FN_empty_array_ptr_deref_bad() {
  std::unique_ptr<int[]> x;
  return x[0];
}

int FN_nullptr_ptr_deref_bad() {
  std::unique_ptr<int> x(nullptr);
  return *x;
}

int FN_nullptr_array_ptr_deref_bad() {
  std::unique_ptr<int[]> x(nullptr);
  return x[2];
}

int FN_empty_ptr_field_deref_bad() {
  std::unique_ptr<X> x;
  return x.get()->field;
}

int FN_empty_ptr_field_deref2_bad() {
  std::unique_ptr<X> x;
  return x->field;
}

int FN_empty_ptr_method_deref_bad() {
  std::unique_ptr<X> x;
  return x->get();
}

int FP_unique_ptr_create_use_ok() {
  std::unique_ptr<X> x(new X());
  return x->get();
}

void unique_ptr_release_bad() {
  std::unique_ptr<X> x(new X());
  x.release();
}


int reset_ptr_null_deref_bad() {
  std::unique_ptr<int> x(new int);
  x.reset();
  return *x;
}

int reset_ptr_null_deref2_bad() {
  std::unique_ptr<int> x(new int);
  x.reset(new int);
  x.reset();
  return *x;
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

int FN_unique_ptr_copy_null_deref_bad() {
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

int FP_unique_ptr_move_deref_ok() {
  std::unique_ptr<int> p1(new int);
  std::unique_ptr<int> p2 = std::move(p1);
  return *p2;
}

int FP_unique_ptr_assign_deref_ok() {
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
