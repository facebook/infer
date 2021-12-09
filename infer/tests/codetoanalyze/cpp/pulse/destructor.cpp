/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace destructor {

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
  int f;
  ~X() {}
};

void destruct_unique_deletes_pointer_bad() {
  X* p = new X;
  { UniquePtr<X> u = UniquePtr<X>(p); }
  int i = p->f;
}

void destruct_unique_ref_deletes_pointer_bad() {
  X* p = new X;
  { const UniquePtr<X>& u = UniquePtr<X>(p); }
  int i = p->f;
}

void destruct_unique_ptr_deletes_pointer_bad() {
  X* p = new X;
  // note: this is not a smart way to use UniquePtr in general
  UniquePtr<X>* u = new UniquePtr<X>(p);
  delete u;
  int i = p->f;
}

} // namespace destructor
