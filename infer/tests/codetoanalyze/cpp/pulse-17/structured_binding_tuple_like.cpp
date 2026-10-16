/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstddef>
#include <utility>

// Structured bindings that decompose a tuple-like type: each binding refers to
// the result of get<I>() on the hidden decomposed object.
namespace structured_binding_tuple_like {

template <class A, class B>
struct Pair {
  A first;
  B second;

  template <std::size_t I>
  auto& get() & {
    if constexpr (I == 0) {
      return first;
    } else {
      return second;
    }
  }

  template <std::size_t I>
  const auto& get() const& {
    if constexpr (I == 0) {
      return first;
    } else {
      return second;
    }
  }

  template <std::size_t I>
  auto&& get() && {
    if constexpr (I == 0) {
      return std::move(first);
    } else {
      return std::move(second);
    }
  }
};

struct RefPair {
  int& first;
  int& second;

  template <std::size_t I>
  int& get() const {
    if constexpr (I == 0) {
      return first;
    } else {
      return second;
    }
  }
};

// get<I>() returns by value: each binding is the temporary holding the result
struct PtrPair {
  int* first;
  int* second;

  template <std::size_t I>
  int* get() const {
    return I == 0 ? first : second;
  }
};

struct Int {
  int value;

  template <std::size_t I>
  int get() const {
    return value;
  }
};

struct Long {
  int value;
};

template <std::size_t I>
int get(const Long& l) {
  return l.value;
}

struct Holder {
  int* ptr;
};

struct HolderSource {
  int* ptr;

  template <std::size_t I>
  Holder get() const {
    return Holder{ptr};
  }
};

struct Derived : Holder {
  int other;
};

struct DerivedSource {
  int* ptr;

  template <std::size_t I>
  Derived get() const {
    return Derived{{ptr}, 0};
  }
};

struct Resetter {
  int** slot;
  ~Resetter() { *slot = nullptr; }
};

struct ResetterSource {
  int** slot;

  template <std::size_t I>
  Resetter get() const {
    return Resetter{slot};
  }
};

struct Owner {
  int* ptr;
  ~Owner() { delete ptr; }
};

struct OwnerSource {
  template <std::size_t I>
  Owner get() const {
    return Owner{new int(0)};
  }
};

} // namespace structured_binding_tuple_like

namespace std {

template <class A, class B>
struct tuple_size<structured_binding_tuple_like::Pair<A, B>>
    : integral_constant<size_t, 2> {};
template <class A, class B>
struct tuple_element<0, structured_binding_tuple_like::Pair<A, B>> {
  using type = A;
};
template <class A, class B>
struct tuple_element<1, structured_binding_tuple_like::Pair<A, B>> {
  using type = B;
};

template <>
struct tuple_size<structured_binding_tuple_like::RefPair>
    : integral_constant<size_t, 2> {};
template <size_t I>
struct tuple_element<I, structured_binding_tuple_like::RefPair> {
  using type = int&;
};

template <>
struct tuple_size<structured_binding_tuple_like::PtrPair>
    : integral_constant<size_t, 2> {};
template <size_t I>
struct tuple_element<I, structured_binding_tuple_like::PtrPair> {
  using type = int*;
};

template <>
struct tuple_size<structured_binding_tuple_like::Int>
    : integral_constant<size_t, 1> {};
template <size_t I>
struct tuple_element<I, structured_binding_tuple_like::Int> {
  using type = int;
};

template <>
struct tuple_size<structured_binding_tuple_like::Long>
    : integral_constant<size_t, 1> {};
template <size_t I>
struct tuple_element<I, structured_binding_tuple_like::Long> {
  using type = long;
};

template <>
struct tuple_size<structured_binding_tuple_like::HolderSource>
    : integral_constant<size_t, 1> {};
template <size_t I>
struct tuple_element<I, structured_binding_tuple_like::HolderSource> {
  using type = structured_binding_tuple_like::Holder;
};

template <>
struct tuple_size<structured_binding_tuple_like::DerivedSource>
    : integral_constant<size_t, 1> {};
template <size_t I>
struct tuple_element<I, structured_binding_tuple_like::DerivedSource> {
  using type = structured_binding_tuple_like::Holder;
};

template <>
struct tuple_size<structured_binding_tuple_like::ResetterSource>
    : integral_constant<size_t, 1> {};
template <size_t I>
struct tuple_element<I, structured_binding_tuple_like::ResetterSource> {
  using type = structured_binding_tuple_like::Resetter;
};

template <>
struct tuple_size<structured_binding_tuple_like::OwnerSource>
    : integral_constant<size_t, 1> {};
template <size_t I>
struct tuple_element<I, structured_binding_tuple_like::OwnerSource> {
  using type = structured_binding_tuple_like::Owner;
};

} // namespace std

namespace structured_binding_tuple_like {

// decomposition of an lvalue reference: the bindings are references to the
// elements

int ref_binding_read_ok() {
  Pair<int, int> p{0, 0};
  auto& [x, y] = p;
  return x + y;
}

int const_ref_binding_read_ok() {
  Pair<int, int> p{0, 0};
  const auto& [x, y] = p;
  return x + y;
}

int forwarding_ref_binding_read_ok() {
  Pair<int, int> p{0, 0};
  auto&& [x, y] = p;
  return x + y;
}

int ref_binding_null_check_ok() {
  Pair<int*, int> p{nullptr, 0};
  const auto& [ptr, n] = p;
  if (ptr != nullptr) {
    return *ptr;
  }
  return n;
}

int ref_binding_deref_valid_ok() {
  int v = 0;
  Pair<int*, int*> p{&v, &v};
  auto& [a, b] = p;
  *a = 1;
  return v;
}

int ref_binding_null_deref_bad() {
  Pair<int*, int> p{nullptr, 0};
  auto& [ptr, n] = p;
  return *ptr;
}

int ref_binding_write_null_bad() {
  int v = 0;
  Pair<int*, int> p{&v, 0};
  auto& [ptr, n] = p;
  ptr = nullptr;
  return *p.first;
}

int ref_binding_write_int_bad() {
  Pair<int, int> p{0, 0};
  auto& [x, y] = p;
  x = 1;
  if (p.first == 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

struct OnePairRange {
  Pair<int, int>* p;
  Pair<int, int>* begin() const { return p; }
  Pair<int, int>* end() const { return p + 1; }
};

void range_for_ref_binding_write_bad() {
  Pair<int, int> p{0, 0};
  for (auto& [x, y] : OnePairRange{&p}) {
    x = 1;
  }
  if (p.first == 1) {
    int* q = nullptr;
    *q = 42;
  }
}

int* address_of_ref_binding(Pair<int, int>& p) {
  auto& [x, y] = p;
  return &y;
}

int deref_address_of_ref_binding_ok() {
  Pair<int, int> p{1, 0};
  int* q = address_of_ref_binding(p);
  return *q;
}

void ref_binding_in_callee(Pair<int, int>& p) { auto& [x, y] = p; }

int read_after_ref_binding_in_callee_ok() {
  Pair<int, int> p{1, 2};
  ref_binding_in_callee(p);
  return p.first;
}

void two_ref_bindings_null_deref_bad() {
  Pair<int, int> p{1, 2};
  {
    auto& [a, b] = p;
  }
  {
    auto& [c, d] = p;
  }
  int* q = nullptr;
  *q = 42;
}

int ref_binding_of_references_read_ok() {
  int i = 0, j = 0;
  RefPair r{i, j};
  auto& [x, y] = r;
  return x + y;
}

// decomposition of an xvalue: the bindings are references to the elements

int xvalue_binding_write_bad() {
  Pair<int, int> p{0, 0};
  auto&& [x, y] = std::move(p);
  x = 1;
  if (p.first == 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

void xvalue_binding_in_callee(Pair<int, int>& p) {
  auto&& [x, y] = std::move(p);
}

int read_after_xvalue_binding_in_callee_ok() {
  Pair<int, int> p{1, 2};
  xvalue_binding_in_callee(p);
  return p.first;
}

void two_xvalue_bindings_null_deref_bad() {
  Pair<int, int> p{1, 2};
  {
    auto&& [a, b] = std::move(p);
  }
  {
    auto&& [c, d] = std::move(p);
  }
  int* q = nullptr;
  *q = 42;
}

// decomposition of a copy: the bindings are aliases for the elements of the
// copy

int value_binding_read_ok() {
  Pair<int, int> p{0, 0};
  auto [x, y] = p;
  return x + y;
}

int value_binding_null_deref_bad() {
  Pair<int*, int> p{nullptr, 0};
  auto [ptr, n] = p;
  return *ptr;
}

// decomposition of a temporary bound to a reference: the bindings are aliases
// for the elements of the temporary

int* address_of_temporary_binding() {
  auto&& [x, y] = Pair<int, int>{1, 2};
  return &y;
}

int deref_address_of_temporary_binding_bad() {
  int* q = address_of_temporary_binding();
  return *q;
}

const int* address_of_const_ref_temporary_binding() {
  const auto& [x, y] = Pair<int, int>{1, 2};
  return &y;
}

int deref_address_of_const_ref_temporary_binding_bad() {
  const int* q = address_of_const_ref_temporary_binding();
  return *q;
}

// the elements of RefPair are references: whatever the decomposition, the
// bindings are references to the objects that the elements refer to

int value_binding_of_references_write_bad() {
  int i = 0, j = 0;
  RefPair r{i, j};
  auto [x, y] = r;
  x = 1;
  if (i == 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int value_binding_of_references_write_ok() {
  int i = 0, j = 0;
  RefPair r{i, j};
  auto [x, y] = r;
  x = 1;
  if (i != 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int temporary_binding_of_references_write_bad() {
  int i = 0, j = 0;
  auto&& [x, y] = RefPair{i, j};
  x = 1;
  if (i == 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

void null_deref_after_bindings_to_same_object_bad() {
  int i = 0;
  RefPair r{i, i};
  auto [x, y] = r;
  int* q = nullptr;
  *q = 42;
}

int value_binding_of_references_in_callee() {
  int i = 0, j = 0;
  RefPair r{i, j};
  auto [x, y] = r;
  x = 1;
  return i;
}

int call_value_binding_of_references_bad() {
  if (value_binding_of_references_in_callee() == 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

int call_value_binding_of_references_ok() {
  if (value_binding_of_references_in_callee() != 1) {
    int* q = nullptr;
    return *q;
  }
  return 0;
}

// get<I>() returns by value

void by_value_get_null_deref_bad() {
  PtrPair p{nullptr, nullptr};
  auto [a, b] = p;
  *a = 1;
}

int by_value_get_not_null_ok() {
  int x = 0;
  PtrPair p{&x, &x};
  auto [a, b] = p;
  *a = 1;
  *b = 2;
  return x;
}

void by_value_get_const_null_deref_bad() {
  PtrPair p{nullptr, nullptr};
  const auto [a, b] = p;
  *a = 1;
}

void by_value_get_const_ref_null_deref_bad() {
  PtrPair p{nullptr, nullptr};
  const auto& [a, b] = p;
  *a = 1;
}

int by_value_get_const_ref_null_check_ok() {
  int x = 0;
  PtrPair p{nullptr, &x};
  const auto& [a, b] = p;
  if (a != nullptr) {
    *a = 1;
  }
  *b = 2;
  return x;
}

int by_value_get_infeasible_branch_ok() {
  Int i{0};
  auto [v] = i;
  int* p = nullptr;
  if (v != 0) {
    return *p;
  }
  return v;
}

int by_value_get_feasible_branch_bad() {
  Int i{0};
  auto [v] = i;
  int* p = nullptr;
  if (v == 0) {
    return *p;
  }
  return v;
}

int free_by_value_get_converted_infeasible_branch_ok() {
  Long l{0};
  auto [v] = l;
  int* p = nullptr;
  if (v != 0) {
    return *p;
  }
  return 0;
}

void by_value_get_class_null_deref_bad() {
  HolderSource s{nullptr};
  auto [h] = s;
  *h.ptr = 1;
}

int by_value_get_class_not_null_ok() {
  int x = 0;
  HolderSource s{&x};
  auto [h] = s;
  *h.ptr = 1;
  return x;
}

void by_value_get_derived_to_base_null_deref_bad() {
  DerivedSource s{nullptr};
  auto [h] = s;
  *h.ptr = 1;
}

void by_value_get_destroyed_at_end_of_scope_bad() {
  int x = 0;
  int* p = &x;
  {
    auto [r] = ResetterSource{&p};
  }
  *p = 1;
}

void by_value_get_not_destroyed_before_end_of_scope_ok() {
  int x = 0;
  int* p = &x;
  auto [r] = ResetterSource{&p};
  *p = 1;
}

int by_value_get_destroyed_ok() {
  auto [o] = OwnerSource{};
  return *o.ptr;
}

// the address of [a] is the address of a temporary in this stack frame, but
// structured bindings are not considered when checking for escaping stack
// addresses
int** FN_by_value_get_return_binding_address_bad() {
  int x = 0;
  PtrPair p{&x, &x};
  auto [a, b] = p;
  return &a;
}

// the hidden decomposed object is destroyed at the end of its scope

int decomposed_object_destroyed_ok() {
  auto [first, second] = Pair<Owner, Owner>{{new int(0)}, {new int(1)}};
  return *first.ptr + *second.ptr;
}

} // namespace structured_binding_tuple_like
