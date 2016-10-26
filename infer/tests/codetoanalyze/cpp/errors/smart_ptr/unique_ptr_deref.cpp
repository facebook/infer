/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <memory>

namespace unique_ptr {

struct X {
  int field;
  int get() { return field; }
  void set(int value) { field = value; }
};

int empty_ptr_access() {
  std::unique_ptr<int> x;
  int* p = x.get(); // no dereference
  if (p) {
    return 1;
  }
  return 0;
}

int empty_ptr_deref() {
  std::unique_ptr<int> x;
  return *x;
}

int empty_array_ptr_deref() {
  std::unique_ptr<int[]> x;
  return x[0];
}

int nullptr_ptr_deref() {
  std::unique_ptr<int> x(nullptr);
  return *x;
}

int nullptr_array_ptr_deref() {
  std::unique_ptr<int[]> x(nullptr);
  return x[2];
}

int empty_ptr_field_deref() {
  std::unique_ptr<X> x;
  return x.get()->field;
}

int empty_ptr_field_deref2() {
  std::unique_ptr<X> x;
  return x->field;
}

int empty_ptr_method_deref() {
  std::unique_ptr<X> x;
  return x->get();
}

int reset_ptr_null_deref() {
  std::unique_ptr<int> x(new int);
  x.reset();
  return *x;
}

int reset_ptr_null_deref2() {
  std::unique_ptr<int> x(new int);
  x.reset(new int);
  x.reset();
  return *x;
}

int reset_ptr_ok_deref() {
  std::unique_ptr<int> x;
  x.reset(new int);
  return *x;
}

int reset_ptr_ok_deref2() {
  std::unique_ptr<int> x;
  x.reset();
  x.reset(new int);
  return *x;
}

int unique_ptr_copy_null_deref() {
  std::unique_ptr<int> p1;
  std::unique_ptr<int> p2 = std::move(p1);
  return *p2;
}

int unique_ptr_assign_null_deref() {
  std::unique_ptr<int> p1(new int);
  std::unique_ptr<int> p2;
  p1 = std::move(p2);
  return *p1;
}

int unique_ptr_move_ok_deref() {
  std::unique_ptr<int> p1(new int);
  std::unique_ptr<int> p2 = std::move(p1);
  return *p2;
}

int unique_ptr_assign_ok_deref() {
  std::unique_ptr<int> p1(new int);
  std::unique_ptr<int> p2;
  p2 = std::move(p1);
  p1.reset();
  return *p2;
}

int unique_ptr_move_null_deref() {
  std::unique_ptr<int> p1(new int);
  std::unique_ptr<int> p2 = std::move(p1);
  return *p1;
}
}
