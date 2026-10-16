/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>
#include <new>

// C++20 parenthesized aggregate initialization

namespace paren_aggregate_init {

struct S {
  int x;
};

struct Agg {
  S* p;
  int n;
};

struct IntThenPtr {
  int n;
  S* p;
};

int null_field_bad() {
  Agg a(nullptr, 1);
  return a.p->x;
}

int nonnull_field_ok() {
  S s{0};
  Agg a(&s, 1);
  return a.p->x;
}

int omitted_pointer_field_is_null_bad() {
  IntThenPtr a(1);
  return a.p->x;
}

int omitted_int_field_is_zero_ok() {
  S s{0};
  Agg a(&s);
  if (a.n != 0) {
    S* q = nullptr;
    return q->x;
  }
  return a.p->x;
}

int unrelated_bug_with_omitted_field_bad() {
  S* q = nullptr;
  Agg a(nullptr);
  return q->x + a.n;
}

int new_null_field_bad() {
  Agg* a = new Agg(nullptr, 1);
  int r = a->p->x;
  delete a;
  return r;
}

int new_array_null_element_bad() {
  S s{0};
  Agg* a = new Agg[2](Agg(&s, 1), Agg(nullptr, 2));
  int r = a[1].p->x;
  delete[] a;
  return r;
}

int new_array_nonnull_element_ok() {
  S s{0};
  Agg* a = new Agg[2](Agg(&s, 1), Agg(nullptr, 2));
  int r = a[0].p->x;
  delete[] a;
  return r;
}

int placement_new_null_field_bad() {
  Agg storage;
  Agg* a = ::new (&storage) Agg(nullptr, 1);
  return a->p->x;
}

template <typename T, typename... Args>
T* make_new(Args... args) {
  return new T(args...);
}

int factory_null_field_bad() {
  S* q = nullptr;
  Agg* a = make_new<Agg>(q, 1);
  int r = a->p->x;
  delete a;
  return r;
}

int deref_field(Agg a) { return a.p->x; }

int temporary_null_field_bad() { return deref_field(Agg(nullptr, 1)); }

int temporary_nonnull_field_ok() {
  S s{0};
  return deref_field(Agg(&s, 1));
}

int array_null_element_bad() {
  S s{0};
  S* arr[2](&s, nullptr);
  return arr[1]->x;
}

int array_nonnull_element_ok() {
  S s{0};
  S* arr[2](&s, nullptr);
  return arr[0]->x;
}

int array_omitted_element_is_null_bad() {
  S s{0};
  S* arr[3](&s);
  return arr[2]->x;
}

int new_array_omitted_element_is_null_bad() {
  S s{0};
  Agg* a = new Agg[3](Agg(&s, 1));
  int r = a[2].p->x;
  delete[] a;
  return r;
}

int array_omitted_record_element_is_null_bad() {
  S s{0};
  Agg arr[2](Agg(&s, 1));
  return arr[1].p->x;
}

struct WithDefault {
  int n;
  S* p = nullptr;
};

int default_member_initializer_null_bad() {
  WithDefault w(1);
  return w.p->x;
}

int default_member_initializer_overridden_ok() {
  S s{0};
  WithDefault w(1, &s);
  return w.p->x;
}

struct Derived : Agg {
  int m;
};

int base_null_field_bad() {
  Derived d(Agg(nullptr, 1), 2);
  return d.p->x;
}

struct BitFields {
  int a : 3;
  int b : 5;
  S* p;
};

int bit_fields_null_field_bad() {
  BitFields b(1, 2, nullptr);
  return b.p->x;
}

struct UniqueHolder {
  std::unique_ptr<S> p;
  int n;
};

int unique_ptr_member_ok() {
  UniqueHolder h(std::unique_ptr<S>(new S{1}), 1);
  return h.p->x;
}

int moved_unique_ptr_member_ok() {
  std::unique_ptr<S> u(new S{1});
  UniqueHolder h(std::move(u), 1);
  return h.p->x;
}

struct Triple {
  int i;
  S* p;
  int k;
};

static int nodeless_branch_helper(int c) {
  Triple t(c, nullptr);
  if (c)
    (void)t.i;
  return t.i;
}

int call_nodeless_branch_helper_bad() {
  nodeless_branch_helper(1);
  S* q = nullptr;
  return q->x;
}

struct Narrow {
  int i;
  S* p;
};

// the frontend stores the unconverted constant 3.7 into the int field, which
// makes the rest of the path infeasible for Pulse
int FN_narrowing_conversion_null_field_bad() {
  Narrow n(3.7, nullptr);
  return n.p->x;
}

struct Holder {
  Agg a;
  Holder() : a(nullptr, 1) {}
};

int member_init_null_field_bad() {
  Holder h;
  return h.a.p->x;
}

struct T {
  int v;
  ~T() {}
};

struct RefHolder {
  const T& t;
};

// unlike brace initialization, parenthesized initialization does not extend
// the lifetime of a temporary bound to a reference member
int ref_member_temporary_not_extended_bad() {
  RefHolder h(T{1});
  return h.t.v;
}

int ref_member_temporary_extended_ok() {
  RefHolder h{T{1}};
  return h.t.v;
}

} // namespace paren_aggregate_init
