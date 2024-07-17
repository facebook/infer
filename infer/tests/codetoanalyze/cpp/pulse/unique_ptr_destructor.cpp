/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

namespace destructor_unique_ptr {
struct X {
  int field;
  int* pointer_field;
  int get() { return field; }
  void set(int value) { field = value; }
  X() { pointer_field = new int; }
  ~X() { delete pointer_field; }
};

struct Y {
  X* pointer_field;
  void leak() { pointer_field = new X(); }
  Y() { pointer_field = new X(); }
  ~Y() { delete pointer_field; }
};

int destructor0_ok() {
  auto x = new std::unique_ptr<int>(new int(5));
  delete x;
  return 0;
}

int destructor1_ok_FP() {
  { std::unique_ptr<int>(new int(5)); }
  return 0;
}

int destructor2_ok() {
  { std::unique_ptr<int> p; }
  return 0;
}

int destructor3_ok() {
  auto x = new std::unique_ptr<int>();
  delete x;
  return 0;
}

int destructor4_ok() {
  auto x = new std::unique_ptr<X>(new X());
  delete x;
  return 0;
}

void destructor5_ok() {
  auto x = new std::unique_ptr<Y>(new Y());
  delete x;
}

int destructor6_ok() {
  auto x = std::unique_ptr<Y>(new Y());
  return 0;
}

void destructor7_ok() {
  auto x = std::unique_ptr<Y>(new Y());
  x.reset();
}

void destructor8_ok() {
  auto x = std::unique_ptr<Y>(new Y());
  x.reset(new Y());
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

X destructor2_bad() {
  auto p = new X();
  { std::unique_ptr<X> x(p); }
  // Should report a NPE here as p has been deallocated
  return *p;
}

int destructor3_bad() {
  auto x = std::unique_ptr<Y>(new Y());
  x->leak(); // Should report a memory leak
  return 0;
}

int destructor4_bad() {
  auto x = std::unique_ptr<Y>(new Y());
  x->leak(); // Should report a memory leak
  x.reset();
  return 0;
}

int destructor5_bad() {
  auto x = std::unique_ptr<Y>(new Y());
  x.reset(new Y());
  x->leak(); // Should report a memory leak
  return 0;
}

} // namespace destructor_unique_ptr
