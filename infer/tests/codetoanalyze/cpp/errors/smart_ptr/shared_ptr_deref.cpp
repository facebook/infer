/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <memory>

namespace shared_ptr {

struct X {
  int field;
  int get() { return field; }
  void set(int value) { field = value; }
};

int empty_ptr_access() {
  std::shared_ptr<int> x;
  int* p = x.get(); // no dereference
  if (p) {
    return 1;
  }
  return 0;
}

int empty_ptr_deref() {
  std::shared_ptr<int> x;
  return *x;
}

int nullptr_ptr_deref() {
  std::shared_ptr<int> x(nullptr);
  return *x;
}

int empty_ptr_field_deref() {
  std::shared_ptr<X> x;
  return x.get()->field;
}

int empty_ptr_field_deref2() {
  std::shared_ptr<X> x;
  return x->field;
}

int empty_ptr_method_deref() {
  std::shared_ptr<X> x;
  return x->get();
}

int reset_ptr_null_deref() {
  std::shared_ptr<int> x(new int);
  x.reset();
  return *x;
}

int reset_ptr_null_deref2() {
  std::shared_ptr<int> x(new int);
  x.reset(new int);
  x.reset();
  return *x;
}

int reset_ptr_ok_deref() {
  std::shared_ptr<int> x;
  x.reset(new int);
  return *x;
}

int reset_ptr_ok_deref2() {
  std::shared_ptr<int> x;
  x.reset();
  x.reset(new int);
  return *x;
}

int shared_ptr_copy_null_deref() {
  std::shared_ptr<int> p1;
  std::shared_ptr<int> p2 = p1;
  return *p2;
}

int shared_ptr_assign_null_deref() {
  std::shared_ptr<int> p1(new int);
  std::shared_ptr<int> p2;
  p1 = p2;
  return *p1;
}

int shared_ptr_copy_ok_deref() {
  std::shared_ptr<int> p1(new int);
  std::shared_ptr<int> p2 = p1;
  return *p2;
}

int shared_ptr_assign_ok_deref() {
  std::shared_ptr<int> p1(new int);
  std::shared_ptr<int> p2;
  p2 = p1;
  p1.reset();
  return *p2;
}

int shared_ptr_move_null_deref() {
  std::shared_ptr<int> p1(new int);
  std::shared_ptr<int> p2 = std::move(p1);
  return *p1;
}

int shared_ptr_check_null() {
  std::shared_ptr<int> p;
  if (p == nullptr)
    return 1;
  return *p;
}

int shared_ptr_check_notnull() {
  std::shared_ptr<int> p;
  if (p != nullptr)
    return *p;
  return 1;
}

int shared_ptr_check_null2(std::shared_ptr<int> p) {
  if (p == nullptr)
    return 1;
  return *p;
}
}
