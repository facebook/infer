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

} // namespace unique_ptr
