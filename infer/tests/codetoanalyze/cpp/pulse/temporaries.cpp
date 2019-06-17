/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace temporaries {

template <typename X>
struct UniquePtr {
  X* x_;
  ~UniquePtr() {
    if (x_) {
      delete x_;
    }
  }
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

struct A {
  int s_;
  ~A() {}
  A() { A(42); }
  A(int s) { s_ = s; }
  A(A& a) { s_ = a.s_; }
};

UniquePtr<A> mk_UniquePtr_A() { return UniquePtr<A>(new A); }

int FN_call_mk_UniquePtr_A_deref_bad() {
  A* a = mk_UniquePtr_A().get(); // temporary unique_ptr returned by
                                 // `mk_UniquePtr_A` is destroyed at the end
                                 // of the statement
  return a->s_;
}

int call_mk_UniquePtr_A_ok() {
  const UniquePtr<A>& local =
      mk_UniquePtr_A(); // ok, as ownership of a temporary unique_ptr is passed
                        // to `local`
  return local->s_;
}

int call_mk_UniquePtr_A_copy_object_ok() {
  A a = *mk_UniquePtr_A().get(); // ok, as value is copied before temporary
                                 // unique_prt is destroyed
  return a.s_;
}

void temporary_in_conditional_ok() {
  while (true) {
    int x = true ? 0 : A(4).s_;
  }
}

void call_mk_UniquePtr_A_get_field_ok() { int x = A().s_; }

int FN_bind_temporary_to_const_bad() {
  A* a_ptr;
  {
    const UniquePtr<A>& local = mk_UniquePtr_A();
    a_ptr = local.get();
  }
  return a_ptr->s_;
}

} // namespace temporaries
