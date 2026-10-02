/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstdlib>

// Structured bindings that decompose a struct or an array name a field or an
// element of the hidden decomposed object.
namespace structured_binding {

struct S {
  int x;
};

struct Lookup {
  int err;
  S* obj;
};

Lookup find_obj(int key) {
  if (key < 0) {
    return {-1, nullptr};
  }
  static S s{1};
  return {0, &s};
}

int struct_value_null_deref_bad() {
  auto [err, obj] = find_obj(-1);
  return obj->x + err;
}

int struct_value_not_null_ok() {
  auto [err, obj] = find_obj(1);
  return obj->x + err;
}

int struct_value_infeasible_path_ok() {
  Lookup l{0, nullptr};
  auto [err, obj] = l;
  int* p = nullptr;
  if (err != 0) {
    return *p;
  }
  return 0;
}

struct Holder {
  int* p;
  int n;
};

int struct_ref_use_after_free_bad() {
  Holder h{(int*)malloc(sizeof(int)), 1};
  if (!h.p) {
    return 0;
  }
  free(h.p);
  auto& [p, n] = h;
  return *p + n;
}

int struct_const_ref_null_deref_bad() {
  Lookup l{0, nullptr};
  const auto& [err, obj] = l;
  return obj->x;
}

int struct_rvalue_ref_null_deref_bad() {
  auto&& [err, obj] = find_obj(-1);
  return obj->x;
}

void write_through_ref_binding_bad() {
  S s{1};
  Lookup l{0, &s};
  auto& [err, obj] = l;
  obj = nullptr;
  l.obj->x = 42;
}

void write_through_value_binding_ok() {
  S s{1};
  Lookup l{0, &s};
  auto [err, obj] = l;
  obj = nullptr; // only modifies the hidden copy of l
  l.obj->x = 42;
}

int address_of_binding_bad() {
  Lookup l{0, nullptr};
  auto& [err, obj] = l;
  S** p = &obj;
  return (*p)->x;
}

struct RefAndPtr {
  int& ref;
  S* obj;
};

int write_through_reference_field_binding_bad() {
  int x = 0;
  auto [ref, obj] = RefAndPtr{x, nullptr};
  ref = 1;
  if (x == 1) {
    int* p = nullptr;
    return *p;
  }
  return 0;
}

int write_through_reference_field_binding_ok() {
  int x = 0;
  auto [ref, obj] = RefAndPtr{x, nullptr};
  ref = 1;
  if (x != 1) {
    int* p = nullptr;
    return *p;
  }
  return 0;
}

struct Base {
  S* obj;
  int n;
};

struct Derived : Base {};

int base_class_fields_bad() {
  Derived d;
  d.obj = nullptr;
  d.n = 0;
  auto [obj, n] = d;
  return obj->x + n;
}

struct WithBitField {
  S* obj;
  int : 3;
  int bits : 5;
};

int unnamed_bit_field_bad() {
  WithBitField w;
  w.obj = nullptr;
  w.bits = 1;
  auto& [obj, bits] = w;
  return obj->x + bits;
}

int array_ref_null_deref_bad() {
  S* arr[2] = {nullptr, nullptr};
  auto& [a, b] = arr;
  return a->x;
}

int array_value_null_deref_bad() {
  S* arr[2] = {nullptr, nullptr};
  auto [a, b] = arr;
  return a->x;
}

bool uninit_field_through_ref_binding_bad() {
  Lookup l;
  auto& [err, obj] = l;
  return obj == nullptr;
}

const int* address_of_temporary_binding() {
  const auto& [p, n] = Holder{nullptr, 1};
  return &n;
}

// the address of a field of the hidden temporary escapes, but Pulse does not
// check whether the addresses of fields of stack objects escape
int FN_deref_address_of_temporary_binding_bad() {
  const int* q = address_of_temporary_binding();
  return *q;
}

// the hidden decomposed object is destroyed at the end of its scope

struct Owner {
  int* p;
  Owner() : p(new int(0)) {}
  ~Owner() { delete p; }
};

struct Owners {
  Owner first;
  Owner second;
};

int decomposed_object_destroyed_ok() {
  auto [first, second] = Owners{};
  return *first.p + *second.p;
}

struct Resetter {
  int** slot;
  ~Resetter() { *slot = nullptr; }
};

struct ResetterAndInt {
  Resetter resetter;
  int n;
};

void decomposed_object_destroyed_at_end_of_scope_bad() {
  int x = 0;
  int* p = &x;
  {
    auto [resetter, n] = ResetterAndInt{{&p}, 0};
  }
  *p = 1;
}

void decomposed_object_not_destroyed_before_end_of_scope_ok() {
  int x = 0;
  int* p = &x;
  auto [resetter, n] = ResetterAndInt{{&p}, 0};
  *p = 1;
}

void decomposed_temporary_destroyed_at_end_of_scope_bad() {
  int x = 0;
  int* p = &x;
  {
    auto&& [resetter, n] = ResetterAndInt{{&p}, 0};
  }
  *p = 1;
}

void decomposed_reference_not_destroyed_ok() {
  int x = 0;
  int* p = &x;
  ResetterAndInt r{{&p}, 0};
  {
    auto& [resetter, n] = r;
  }
  *p = 1;
}

} // namespace structured_binding
