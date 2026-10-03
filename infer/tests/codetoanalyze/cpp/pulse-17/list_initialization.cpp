/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

namespace list_initialization {

// since C++17, braces around a prvalue of the same class type initialize the
// object directly from the prvalue, as parentheses would
struct single_field_owner {
  explicit single_field_owner(int* p) : p_(p) {}
  ~single_field_owner() { delete p_; }
  single_field_owner(const single_field_owner&) = delete;
  single_field_owner(single_field_owner&&) = delete;
  int get() const { return *p_; }
  int* p_;
};

single_field_owner make_single_field_owner() {
  return single_field_owner(new int(42));
}

void init_single_field_owner_ok() {
  single_field_owner o{make_single_field_owner()};
}

void const_init_single_field_owner_ok() {
  const single_field_owner o{make_single_field_owner()};
}

void copy_list_init_single_field_owner_ok() {
  auto o = single_field_owner{make_single_field_owner()};
}

int temporary_single_field_owner_ok() {
  return *single_field_owner{make_single_field_owner()}.p_;
}

int temporary_single_field_owner_method_ok() {
  return single_field_owner{make_single_field_owner()}.get();
}

struct single_field_owner_holder {
  single_field_owner_holder() : o_{make_single_field_owner()} {}
  single_field_owner o_;
};

void member_init_single_field_owner_ok() { single_field_owner_holder h; }

struct single_field_owner_default_member {
  single_field_owner_default_member() {}
  single_field_owner o_{make_single_field_owner()};
};

void default_member_init_single_field_owner_ok() {
  single_field_owner_default_member h;
}

struct single_field_owner_aggregate {
  single_field_owner o;
};

void nested_braces_single_field_owner_ok() {
  single_field_owner_aggregate a{{make_single_field_owner()}};
}

void new_single_field_owner_ok() {
  single_field_owner* o = new single_field_owner{make_single_field_owner()};
  delete o;
}

single_field_owner return_braces_single_field_owner() {
  return {make_single_field_owner()};
}

void init_from_return_braces_single_field_owner_ok() {
  single_field_owner o = return_braces_single_field_owner();
}

int use_single_field_owner(single_field_owner o) { return *o.p_; }

int arg_braces_single_field_owner_ok() {
  return use_single_field_owner(single_field_owner{make_single_field_owner()});
}

struct single_ptr_field {
  int* p;
};

single_ptr_field get_null_single_ptr_field() {
  return single_ptr_field{nullptr};
}

int init_single_ptr_field_from_call_bad() {
  single_ptr_field s{get_null_single_ptr_field()};
  return *s.p;
}

int init_single_ptr_field_nested_braces_bad() {
  single_ptr_field s{single_ptr_field{nullptr}};
  return *s.p;
}

struct single_ptr_field_aggregate {
  single_ptr_field f;
};

int init_single_ptr_field_aggregate_nested_braces_bad() {
  single_ptr_field_aggregate a{{get_null_single_ptr_field()}};
  return *a.f.p;
}

int init_single_ptr_field_bad() {
  single_ptr_field s{nullptr};
  return *s.p;
}

int init_single_ptr_field_ok() {
  int x = 42;
  single_ptr_field s{&x};
  return *s.p;
}

int ref_init_single_ptr_field_bad() {
  single_ptr_field s{nullptr};
  single_ptr_field& r{s};
  return *r.p;
}

int const_ref_init_single_ptr_field_bad() {
  const single_ptr_field& r{get_null_single_ptr_field()};
  return *r.p;
}

int rvalue_ref_init_single_ptr_field_bad() {
  single_ptr_field&& r{get_null_single_ptr_field()};
  return *r.p;
}

int ref_init_array_bad() {
  int* a[2] = {nullptr, nullptr};
  int*(&r)[2]{a};
  return *r[0];
}

int* get_null_ptr() { return nullptr; }

int function_ref_braces_bad() {
  int* (&f)(){get_null_ptr};
  return *f();
}

int lambda_single_capture_braces_bad() {
  int* p = nullptr;
  auto f{[p]() { return *p; }};
  return f();
}

struct empty_base {};

struct no_field_derived : empty_base {};

no_field_derived make_no_field_derived(int* p) {
  *p = 42;
  return no_field_derived{};
}

void init_no_field_derived_bad() {
  no_field_derived d{make_no_field_derived(nullptr)};
}

std::unique_ptr<int> make_null_unique_ptr() { return nullptr; }

int init_unique_ptr_from_call_bad() {
  std::unique_ptr<int> p{make_null_unique_ptr()};
  return *p;
}

int init_unique_ptr_from_make_unique_ok() {
  std::unique_ptr<int> p{std::make_unique<int>(42)};
  return *p;
}

} // namespace list_initialization
