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

struct ByReference {
  template <std::size_t I>
  int& get();
};

} // namespace structured_binding

namespace std {
template <>
struct tuple_size<structured_binding::ByReference>
    : integral_constant<size_t, 2> {};
template <size_t I>
struct tuple_element<I, structured_binding::ByReference> {
  using type = int;
};
} // namespace std

namespace structured_binding {

int capture_aggregate_bindings(Point& p) {
  auto& [x, y] = p;
  auto f = [x, &y]() { return x + y; };
  return f();
}

int capture_tuple_like_bindings(ByReference& t) {
  auto& [x, y] = t;
  auto f = [x, &y]() { return x + y; };
  return f();
}

} // namespace structured_binding
