/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <cstddef>
#include <utility>

namespace structured_binding {

struct Point {
  int x;
  int y;
};

int decompose_struct_by_value(Point p) {
  auto [x, y] = p;
  x = 42;
  return x + y;
}

int decompose_struct_by_ref(Point& p) {
  auto& [x, y] = p;
  x = 42;
  return x + y;
}

int decompose_array_by_ref(int (&a)[2]) {
  auto& [x, y] = a;
  return x + y;
}

int decompose_in_range_for(Point (&a)[2]) {
  int r = 0;
  for (const auto& [x, y] : a) {
    r += x + y;
  }
  return r;
}

typedef int int2 __attribute__((vector_size(8)));

int decompose_vector(int2 v) {
  auto [x, y] = v;
  return x + y;
}

struct Destructible {
  int x;
  ~Destructible();
};

struct DestructiblePoint {
  Destructible d;
  int y;
};

int decomposed_object_destroyed(DestructiblePoint p) {
  auto [d, y] = p;
  return d.x + y;
}

void cleanup_point(void* p);

int decomposed_object_cleanup(Point p) {
  __attribute__((cleanup(cleanup_point))) auto [x, y] = p;
  return x + y;
}

struct ByReference {
  template <std::size_t I>
  int& get();
};

struct ByValue;

} // namespace structured_binding

namespace std {
template <>
struct tuple_size<structured_binding::ByReference>
    : integral_constant<size_t, 1> {};
template <size_t I>
struct tuple_element<I, structured_binding::ByReference> {
  using type = int;
};

template <>
struct tuple_size<structured_binding::ByValue> : integral_constant<size_t, 2> {
};
template <>
struct tuple_element<0, structured_binding::ByValue> {
  using type = int;
};
template <>
struct tuple_element<1, structured_binding::ByValue> {
  using type = structured_binding::Destructible;
};
} // namespace std

namespace structured_binding {

struct ByValue {
  template <std::size_t I>
  std::tuple_element_t<I, ByValue> get();
};

int tuple_like_by_value(ByReference t) {
  auto [i] = t;
  i = 42;
  return i;
}

int tuple_like_by_ref(ByReference& t) {
  auto& [i] = t;
  i = 42;
  return i;
}

int tuple_like_by_rvalue_ref(ByReference& t) {
  auto&& [i] = static_cast<ByReference&&>(t);
  i = 42;
  return i;
}

int tuple_like_by_value_get(ByValue& t) {
  auto& [i, d] = t;
  return i + d.x;
}

} // namespace structured_binding
