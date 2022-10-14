/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <functional>
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

void simple_new_array_delete_array_ok() {
  X* x = new X[5];
  delete[] x;
}

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmismatched-new-delete"
void new_array_delete_bad() {
  X* x = new X[5];
  delete x;
}
#pragma clang diagnostic pop

void unique_ptr_managed_ok() {
  struct UniquePtr<X> stack_allocated {
    new X
  };
  const struct UniquePtr<X>& const_ref { new X };
}

void unique_ptr_pointer_bad() {
  struct UniquePtr<X>* heap_allocated = new UniquePtr<X>(new X);
}

void static_local_alloc_ok() { static X* x = new X; }

void call_static_local_alloc_ok() { static_local_alloc_ok(); }

// the initialization of the static local is run every time the function is
// called instead of at most once, causing the FP
void FP_call_static_local_alloc_twice_ok() {
  static_local_alloc_ok();
  static_local_alloc_ok();
}

static UniquePtr<X>* global_pointer;

void global_alloc_ok() { global_pointer = new UniquePtr<X>(new X); }

void unknown(void* x);

void unknown_wrapper(UniquePtr<X>* x) { unknown(x); }

void unknown_alloc_ok() {
  UniquePtr<X>* x = new UniquePtr<X>(new X);
  // could do anything, including deallocate x, so best not to report anything
  unknown(x);
}

void unknown_wrapper_alloc_ok() {
  UniquePtr<X>* x = new UniquePtr<X>(new X);
  // same as above, inter-procedurally
  unknown_wrapper(x);
}

void unknown_wrapper_alloc_then_leak_bad() {
  UniquePtr<X>* x = new UniquePtr<X>(new X);
  unknown_wrapper(x);
  x->x_ = new X;
}

struct Y {
  int* a;
};

void deep_alloc_unknown_ok(Y* y) {
  y->a = new int;
  unknown(y);
  y->a = new int;
}

void call_deep_alloc_unknown_ok() {
  Y* y = new Y;
  deep_alloc_unknown_ok(y);
  delete y->a;
  delete y;
}

void store_closure_unknown(const std::function<void()>& f);

void capture_alloc_unknown_ok() {
  X* x = new X;
  store_closure_unknown([&] { delete x; });
}

std::function<void(void)> capture_alloc_return_ok() {
  X* x = new X;
  return [=]() { delete x; };
}

} // namespace leaks
