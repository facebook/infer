/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#pragma clang diagnostic ignored "-Wc++17-extensions"

namespace std {
template <class T>
struct tuple_size;
template <unsigned long I, class T>
struct tuple_element;
} // namespace std

struct Point {
  int x, y;
};

struct Pair {
  template <unsigned long I>
  int &get();
};

namespace std {
template <>
struct tuple_size<Pair> {
  static const unsigned long value = 2;
};
template <unsigned long I>
struct tuple_element<I, Pair> {
  typedef int type;
};
} // namespace std

int f(Point p, int (&a)[2], Pair t) {
  auto [x, y] = p;
  auto &[b, c] = a;
  auto [i, j] = t;
  return x + b + i;
}
