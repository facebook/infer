/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <new>

namespace leaks {

template <typename X>
struct UniquePtr {
  X* x_;
  ~UniquePtr() { delete x_; }
  UniquePtr(X* y) { x_ = y; }

  UniquePtr(UniquePtr<X>& p) = delete; // no copy constructor
  UniquePtr(UniquePtr<X>&& p) {
    x_ = p.get();
    p.x_ = nullptr;
  }

  X* get() const { return x_; }

  X& operator*() const { return *get(); }

  X* operator->() const { return get(); }
};

struct X {
  int i;
  ~X() {}
};

void simple_new_bad() { X* x = new X; }

void simple_new_delete_ok() {
  X* x = new X;
  delete x;
}

void new_placement_new_delete_ok() {
  X* x = new X;
  X* y = new (x) X();
  delete y;
}

void simple_new_array_bad() { X* x = new X[5]; }

void FP_simple_new_array_delete_array_ok() {
  X* x = new X[5];
  delete[] x;
}

void FN_new_array_delete_bad() {
  X* x = new X[5];
  delete x;
}

void unique_ptr_managed_ok() {
  struct UniquePtr<X> stack_allocated {
    new X
  };
  const struct UniquePtr<X>& const_ref { new X };
}

void unique_ptr_pointer_bad() {
  struct UniquePtr<X>* heap_allocated = new UniquePtr<X>(new X);
}

} // namespace leaks
