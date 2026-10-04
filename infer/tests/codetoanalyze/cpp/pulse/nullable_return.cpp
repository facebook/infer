/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// cpp/pulse-no-nullability-annotations analyzes this file without
// --pulse-nullability-annotations

namespace nullable_return {

struct Obj {
  int x;
};

// declared but not defined: calls to these functions are unknown to Pulse
Obj* _Nullable find(int key);
typedef Obj* _Nullable NullableObj;
NullableObj find_typedef(int key);
using NullableObjAlias = Obj* _Nullable;
NullableObjAlias find_alias(int key);
using NullableObjTwoLevelAlias = NullableObj;
NullableObjTwoLevelAlias find_two_level_alias(int key);
using NonnullObjAlias = Obj* _Nonnull;
using NullableObjFinder = Obj* _Nullable(int key) const;
template <class T>
using NullablePtr = T* _Nullable;
NullablePtr<Obj> find_alias_template(int key);
template <class T>
using NullableImpl = T _Nullable;
template <class T>
using Nullable = NullableImpl<T>;
Nullable<Obj*> find_two_level_alias_template(int key);
template <class T>
using Nonnull = T _Nonnull;
Nonnull<Obj*> get_alias_template(int key);
namespace detail {
typedef Obj* _Nullable DetailNullableObj;
} // namespace detail
using detail::DetailNullableObj;
DetailNullableObj find_using_declaration(int key);
decltype(find) find_decltype;

class Registry {
 public:
  Obj* _Nullable lookup(int key) const;
  Obj* _Nonnull get(int key) const;
  [[gnu::returns_nonnull]] Obj* get_attribute(int key) const;
  NullableObjAlias lookup_alias(int key) const;
  NonnullObjAlias get_alias(int key) const;
  NullableObjFinder lookup_declared_by_alias;
  static Obj* _Nullable lookup_static(int key);
  virtual Obj* _Nullable lookup_virtual(int key);
};

class Handle {
 public:
  Obj* _Nullable operator->() const;
  operator Obj* _Nullable() const;
};

int nullable_function_deref_bad() { return find(1)->x; }

int nullable_method_deref_bad(const Registry& r) { return r.lookup(1)->x; }

int nullable_static_method_deref_bad() { return Registry::lookup_static(1)->x; }

int nullable_virtual_method_deref_bad(Registry* r) {
  return r->lookup_virtual(1)->x;
}

int nullable_arrow_operator_deref_bad(const Handle& h) { return h->x; }

int nullable_conversion_operator_deref_bad(const Handle& h) {
  Obj* o = h;
  return o->x;
}

int nullable_method_checked_ok(const Registry& r) {
  Obj* o = r.lookup(1);
  if (o == nullptr) {
    return 0;
  }
  return o->x;
}

int nonnull_method_null_branch_ok(const Registry& r) {
  Obj* o = r.get(1);
  if (o == nullptr) {
    int* leaked = new int;
    return 0;
  }
  return o->x;
}

int nonnull_attribute_method_null_branch_ok(const Registry& r) {
  Obj* o = r.get_attribute(1);
  if (o == nullptr) {
    int* leaked = new int;
    return 0;
  }
  return o->x;
}

int nullable_typedef_return_deref_bad() { return find_typedef(1)->x; }

int nullable_alias_return_deref_bad() { return find_alias(1)->x; }

int nullable_two_level_alias_return_deref_bad() {
  return find_two_level_alias(1)->x;
}

int nullable_alias_template_return_deref_bad() {
  return find_alias_template(1)->x;
}

int nullable_two_level_alias_template_return_deref_bad() {
  return find_two_level_alias_template(1)->x;
}

int nonnull_alias_template_return_null_branch_ok() {
  Obj* o = get_alias_template(1);
  if (o == nullptr) {
    int* leaked = new int;
    return 0;
  }
  return o->x;
}

int nullable_using_declaration_return_deref_bad() {
  return find_using_declaration(1)->x;
}

int nullable_decltype_declared_return_deref_bad() {
  return find_decltype(1)->x;
}

int nullable_alias_method_deref_bad(const Registry& r) {
  return r.lookup_alias(1)->x;
}

int nullable_method_declared_by_alias_deref_bad(const Registry& r) {
  return r.lookup_declared_by_alias(1)->x;
}

int nonnull_alias_method_null_branch_ok(const Registry& r) {
  Obj* o = r.get_alias(1);
  if (o == nullptr) {
    int* leaked = new int;
    return 0;
  }
  return o->x;
}

} // namespace nullable_return
