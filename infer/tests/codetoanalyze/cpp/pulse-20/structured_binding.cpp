/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstddef>
#include <utility>

// lambdas can capture structured bindings since C++20
namespace structured_binding {

struct S {
  int x;
};

struct Aggregate {
  S* first;
  S* second;
};

struct RefAndPtr {
  int& ref;
  S* obj;
};

struct TupleLike {
  S* first;
  S* second;

  template <std::size_t I>
  S*& get() {
    return I == 0 ? first : second;
  }
};

struct ByValueGet {
  S* first;

  template <std::size_t I>
  S* get() const {
    return first;
  }
};

} // namespace structured_binding

namespace std {
template <>
struct tuple_size<structured_binding::TupleLike>
    : integral_constant<size_t, 2> {};
template <size_t I>
struct tuple_element<I, structured_binding::TupleLike> {
  using type = structured_binding::S*;
};

template <>
struct tuple_size<structured_binding::ByValueGet>
    : integral_constant<size_t, 1> {};
template <size_t I>
struct tuple_element<I, structured_binding::ByValueGet> {
  using type = structured_binding::S*;
};
} // namespace std

namespace structured_binding {

int aggregate_capture_by_value_null_deref_bad() {
  Aggregate p{nullptr, nullptr};
  auto [a, b] = p;
  auto f = [a]() { return a->x; };
  return f();
}

int aggregate_capture_by_value_not_null_ok() {
  S s{1};
  Aggregate p{&s, nullptr};
  auto [a, b] = p;
  auto f = [a]() { return a->x; };
  return f();
}

int aggregate_capture_by_ref_null_deref_bad() {
  Aggregate p{nullptr, nullptr};
  auto& [a, b] = p;
  auto f = [&a]() { return a->x; };
  return f();
}

int aggregate_capture_by_ref_write_bad() {
  S s{1};
  Aggregate p{&s, &s};
  auto& [a, b] = p;
  auto f = [&a]() { a = nullptr; };
  f();
  return p.first->x;
}

int aggregate_init_capture_null_deref_bad() {
  Aggregate p{nullptr, nullptr};
  auto [a, b] = p;
  auto f = [c = a]() { return c->x; };
  return f();
}

int aggregate_nested_capture_null_deref_bad() {
  Aggregate p{nullptr, nullptr};
  auto [a, b] = p;
  auto f = [a]() {
    auto g = [a]() { return a->x; };
    return g();
  };
  return f();
}

int binding_shadowing_capture_null_deref_bad() {
  S s{1};
  Aggregate p{&s, &s};
  auto [a, b] = p;
  auto f = [a](Aggregate q) {
    {
      auto [a, b] = q;
      return a->x;
    }
  };
  return f(Aggregate{nullptr, nullptr});
}

int binding_shadowing_capture_ok() {
  S s{1};
  Aggregate p{nullptr, nullptr};
  auto [a, b] = p;
  auto f = [a](Aggregate q) {
    {
      auto [a, b] = q;
      return a->x;
    }
  };
  return f(Aggregate{&s, &s});
}

int reference_field_capture_by_value_ok() {
  int x = 1;
  auto [ref, obj] = RefAndPtr{x, nullptr};
  auto f = [ref]() { return ref; };
  if (f() != 1) {
    int* p = nullptr;
    return *p;
  }
  return 0;
}

int reference_field_capture_by_ref_write_bad() {
  int x = 0;
  auto [ref, obj] = RefAndPtr{x, nullptr};
  auto f = [&ref]() { ref = 1; };
  f();
  if (x == 1) {
    int* p = nullptr;
    return *p;
  }
  return 0;
}

int tuple_like_ref_capture_by_value_not_null_ok() {
  S s{1};
  TupleLike p{&s, nullptr};
  auto& [a, b] = p;
  auto f = [a]() { return a->x; };
  return f();
}

int tuple_like_ref_capture_by_ref_write_bad() {
  S s{1};
  TupleLike p{&s, &s};
  auto& [a, b] = p;
  auto f = [&a]() { a = nullptr; };
  f();
  return p.first->x;
}

int by_value_get_capture_by_value_null_deref_bad() {
  ByValueGet p{nullptr};
  auto [a] = p;
  auto f = [a]() { return a->x; };
  return f();
}

} // namespace structured_binding
